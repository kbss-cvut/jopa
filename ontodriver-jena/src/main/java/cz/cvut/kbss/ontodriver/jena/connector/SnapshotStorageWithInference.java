/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfigParam;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.jena.exception.ReasonerInitializationException;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.rdf.model.InfModel;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.reasoner.IllegalParameterException;
import org.apache.jena.reasoner.Reasoner;
import org.apache.jena.reasoner.ReasonerFactory;
import org.apache.jena.reasoner.ValidityReport;
import org.apache.jena.system.Txn;
import org.apache.jena.vocabulary.ReasonerVocabulary;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;
import java.util.stream.Collectors;

import static org.apache.jena.rdf.model.ResourceFactory.createProperty;

class SnapshotStorageWithInference extends SnapshotStorage {

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

    private final Map<String, InfModel> inferredGraphs = new HashMap<>();

    SnapshotStorageWithInference(DriverConfiguration configuration, Map<String, String> reasonerConfig) {
        super(configuration);
        this.reasonerFactory = initReasonerFactory(configuration);
        this.reasonerConfig = reasonerConfig.entrySet().stream()
                                            .filter(e -> SUPPORTED_CONFIG.contains(e.getKey()))
                                            .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        this.dataset = DatasetFactory.createGeneral();
    }

    private ReasonerFactory initReasonerFactory(DriverConfiguration configuration) {
        final String factoryClass = configuration.getProperty(DriverConfigParam.REASONER_FACTORY_CLASS, "");
        LOG.trace("Creating reasoner using reasoner factory class {}.", factoryClass);
        try {
            final Class<? extends ReasonerFactory> rfClass =
                    (Class<? extends ReasonerFactory>) Class.forName(factoryClass);
            final Method instanceMethod = rfClass.getMethod("theInstance");
            return (ReasonerFactory) instanceMethod.invoke(null);
        } catch (ClassNotFoundException e) {
            throw new ReasonerInitializationException("Reasoner factory class " + factoryClass + " not found.", e);
        } catch (NoSuchMethodException e) {
            throw new ReasonerInitializationException("Class " + factoryClass +
                    " is not a ReasonerFactory implementation or does not contain static 'theInstance' method.");
        } catch (IllegalAccessException | InvocationTargetException e) {
            throw new ReasonerInitializationException(
                    "Unable to instantiate Jena reasoner from factory " + factoryClass, e);
        }
    }

    @Override
    void addCentralData(Dataset central) {
        Txn.executeRead(central, () -> {
            final Iterator<String> it = central.listNames();
            InfModel clonedModel;
            while (it.hasNext()) {
                final String name = it.next();
                clonedModel = cloneModel(central.getNamedModel(name));
                inferredGraphs.put(name, clonedModel);
                dataset.addNamedModel(name, clonedModel);
            }
            clonedModel = cloneModel(central.getDefaultModel());
            inferredGraphs.put(null, clonedModel);
            dataset.setDefaultModel(clonedModel);
        });
    }

    private InfModel cloneModel(Model model) {
        return ModelFactory.createInfModel(createReasoner(), ModelFactory.createDefaultModel().add(model));
    }

    @Override
    public InfModel getDefaultGraph() {
        return inferredGraphs.get(null);
    }

    private Reasoner createReasoner() {
        final Reasoner reasoner = reasonerFactory.create(null);
        reasonerConfig.forEach((key, value) -> {
            final Property prop = createProperty(key);
            try {
                reasoner.setParameter(prop, value);
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
    public InfModel getNamedGraph(String context) {
        return inferredGraphs.computeIfAbsent(context, c -> {
            // If the context does not exist, we need to create it, so that the default Dataset behavior is preserved
            final InfModel model = ModelFactory.createInfModel(createReasoner(), ModelFactory.createDefaultModel());
            dataset.addNamedModel(context, model);
            return model;
        });
    }

    Model getRawNamedGraph(String context) {
        return inferredGraphs.containsKey(context) ? inferredGraphs.get(context).getRawModel() :
               dataset.getNamedModel(context);
    }

    ValidityReport checkConsistency(String context) {
        return context != null ? getNamedGraph(context).validate() : getDefaultGraph().validate();
    }
}
