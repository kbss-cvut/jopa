package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.ConfigParam;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.jena.exception.ReasonerInitializationException;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.rdf.model.InfModel;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.reasoner.IllegalParameterException;
import org.apache.jena.reasoner.Reasoner;
import org.apache.jena.reasoner.ReasonerFactory;
import org.apache.jena.reasoner.ValidityReport;
import org.apache.jena.vocabulary.ReasonerVocabulary;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;

import static org.apache.jena.rdf.model.ResourceFactory.createProperty;

class SnapshotStorageWithInference extends SnapshotStorage {

    private static final Logger LOG = LoggerFactory.getLogger(SnapshotStorageWithInference.class);

    /**
     * Configuration parameters supported by at least one of the Jena reasoners. Used to pre-filter reasoner config.
     */
    private static final Set<String> SUPPORTED_CONFIG = new HashSet<>(Arrays.asList(
            ReasonerVocabulary.PROPderivationLogging.getURI(),
            ReasonerVocabulary.PROPenableCMPScan.getURI(),
            ReasonerVocabulary.PROPenableFunctorFiltering.getURI(),
            ReasonerVocabulary.PROPenableOWLTranslation.getURI(),
            ReasonerVocabulary.PROPenableTGCCaching.getURI(),
            ReasonerVocabulary.PROPruleMode.getURI(),
            ReasonerVocabulary.PROPruleSet.getURI(),
            ReasonerVocabulary.PROPsetRDFSLevel.getURI(),
            ReasonerVocabulary.PROPtraceOn.getURI()
    ));

    private final ReasonerFactory reasonerFactory;
    private final Map<String, String> reasonerConfig;

    private Map<String, InfModel> inferredGraphs = new HashMap<>();

    SnapshotStorageWithInference(Configuration configuration, Map<String, String> reasonerConfig) {
        super(configuration);
        this.reasonerFactory = initReasonerFactory(configuration);
        this.reasonerConfig = reasonerConfig;
    }

    private ReasonerFactory initReasonerFactory(Configuration configuration) {
        final String factoryClass = configuration.getProperty(ConfigParam.REASONER_FACTORY_CLASS, "");
        LOG.trace("Creating reasoner using reasoner factory class {}.", factoryClass);
        try {
            final Class<? extends ReasonerFactory> rfClass =
                    (Class<? extends ReasonerFactory>) Class.forName(factoryClass);
            final Method instanceMethod = rfClass.getMethod("theInstance");
            return (ReasonerFactory) instanceMethod.invoke(null);
        } catch (ClassNotFoundException e) {
            throw new ReasonerInitializationException("Reasoner factory class " + factoryClass + " not found.", e);
        } catch (NoSuchMethodException e) {
            throw new ReasonerInitializationException(
                    "Class " + factoryClass + " is not a ReasonerFactory implementation or does not contain static \'theInstance\' method.");
        } catch (IllegalAccessException | InvocationTargetException e) {
            throw new ReasonerInitializationException(
                    "Unable to instantiate Jena reasoner from factory " + factoryClass, e);
        }
    }

    @Override
    void initialize() {
        this.dataset = DatasetFactory.createGeneral();
    }

    @Override
    InfModel getDefaultGraph() {
        if (inferredGraphs.containsKey(null)) {
            return inferredGraphs.get(null);
        } else {
            // This does not behave the same as other storages - when defaultAsUnion is set, it uses the currently held
            // versions of the named graphs, which may be without inference. It should probably initialize reasoners for
            // all named graphs in the dataset.
            final InfModel model = ModelFactory.createInfModel(createReasoner(), dataset.getDefaultModel());
            dataset.setDefaultModel(model);
            inferredGraphs.put(null, model);
            return model;
        }
    }

    private Reasoner createReasoner() {
        final Reasoner reasoner = reasonerFactory.create(null);
        reasonerConfig.entrySet().stream().filter(e -> SUPPORTED_CONFIG.contains(e.getKey())).forEach(e -> {
            final Property prop = createProperty(e.getKey());
            try {
                reasoner.setParameter(prop, e.getValue());
            } catch (IllegalParameterException ex) {
                LOG.error("Failed to set property " + prop + " on reasoner.", ex);
            }
        });
        return reasoner;
    }

    Model getRawDefaultGraph() {
        return inferredGraphs.containsKey(null) ? inferredGraphs.get(null).getRawModel() : dataset.getDefaultModel();
    }

    @Override
    InfModel getNamedGraph(String context) {
        if (inferredGraphs.containsKey(context)) {
            return inferredGraphs.get(context);
        } else {
            final InfModel model =
                    ModelFactory.createInfModel(createReasoner(), dataset.getNamedModel(context));
            dataset.addNamedModel(context, model);
            inferredGraphs.put(context, model);
            return model;
        }
    }

    Model getRawNamedGraph(String context) {
        return inferredGraphs.containsKey(context) ? inferredGraphs.get(context).getRawModel() :
                dataset.getNamedModel(context);
    }

    ValidityReport checkConsistency(String context) {
        return context != null ? getNamedGraph(context).validate() : getDefaultGraph().validate();
    }
}
