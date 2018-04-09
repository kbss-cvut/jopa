package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.ConfigParam;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.jena.exception.ReasonerInitializationException;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.rdf.model.InfModel;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.reasoner.ReasonerFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

class SnapshotStorageWithInference extends SnapshotStorage {

    private static final Logger LOG = LoggerFactory.getLogger(SnapshotStorageWithInference.class);

    private final ReasonerFactory reasonerFactory;
    private Map<String, InfModel> inferredGraphs = new HashMap<>();

    SnapshotStorageWithInference(Configuration configuration) {
        super(configuration);
        this.reasonerFactory = initReasonerFactory(configuration);
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
    Model getDefaultGraph() {
        if (inferredGraphs.containsKey(null)) {
            return super.getDefaultGraph();
        } else {
            // This does not behave the same as other storages - when defaultAsUnion is set, it uses the currently held
            // versions of the named graphs, which may be without inference. It should probably initialize reasoners for
            // all named graphs in the dataset.
            final InfModel model = ModelFactory.createInfModel(reasonerFactory.create(null), dataset.getDefaultModel());
            dataset.setDefaultModel(model);
            inferredGraphs.put(null, model);
            return model;
        }
    }

    Model getRawDefaultGraph() {
        return inferredGraphs.containsKey(null) ? inferredGraphs.get(null).getRawModel() : dataset.getDefaultModel();
    }

    @Override
    Model getNamedGraph(String ctx) {
        if (inferredGraphs.containsKey(ctx)) {
            return inferredGraphs.get(ctx);
        } else {
            final InfModel model =
                    ModelFactory.createInfModel(reasonerFactory.create(null), dataset.getNamedModel(ctx));
            dataset.addNamedModel(ctx, model);
            inferredGraphs.put(ctx, model);
            return model;
        }
    }

    Model getRawNamedGraph(String ctx) {
        return inferredGraphs.containsKey(ctx) ? inferredGraphs.get(ctx).getRawModel() : dataset.getNamedModel(ctx);
    }
}
