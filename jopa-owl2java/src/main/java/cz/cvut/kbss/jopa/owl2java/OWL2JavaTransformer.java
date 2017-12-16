/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.owl2java;

import cz.cvut.kbss.jopa.model.SequencesVocabulary;
import cz.cvut.kbss.jopa.owl2java.exception.OWL2JavaException;
import cz.cvut.kbss.jopa.util.MappingFileParser;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.OWLOntologyMerger;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.net.URI;
import java.util.*;

public class OWL2JavaTransformer {

    private static final Logger LOG = LoggerFactory.getLogger(OWL2JavaTransformer.class);

    private static final ContextDefinition DEFAULT_CONTEXT = new ContextDefinition("<DEFAULT>");

    private static final List<IRI> skipped = Arrays
            .asList(IRI.create(SequencesVocabulary.c_Collection), IRI.create(SequencesVocabulary.c_List),
                    IRI.create(SequencesVocabulary.c_OWLSimpleList),
                    IRI.create(SequencesVocabulary.c_OWLReferencedList));

    private final ValidContextAnnotationValueVisitor v = new ValidContextAnnotationValueVisitor();
    private OWLOntology ontology;
    private Map<String, ContextDefinition> contexts = new HashMap<>();
    private boolean ignoreMissingImports;

    public Collection<String> listContexts() {
        return contexts.keySet();
    }

    public void ignoreMissingImports(boolean ignore) {
        this.ignoreMissingImports = ignore;
    }

    private OWLOntology getWholeOntology(final String owlOntologyName, final String mappingFile) {
        // reader
        final OWLOntologyManager m = OWLManager.createOWLOntologyManager();

        if (mappingFile != null) {
            LOG.info("Using mapping file '{}'.", mappingFile);

            final Map<URI, URI> map = MappingFileParser.getMappings(new File(mappingFile));
            m.addIRIMapper(ontologyIRI -> {
                final URI value = map.get(ontologyIRI.toURI());

                if (value == null) {
                    return null;
                } else {
                    return IRI.create(value);
                }
            });
            LOG.info("Mapping file successfully parsed.");
        }

        LOG.info("Loading ontology {} ... ", owlOntologyName);
        if (ignoreMissingImports) {
            m.setSilentMissingImportsHandling(true);
        }

        try {
            m.loadOntology(org.semanticweb.owlapi.model.IRI.create(owlOntologyName));
            return new OWLOntologyMerger(m)
                    .createMergedOntology(m, org.semanticweb.owlapi.model.IRI.create(owlOntologyName + "-generated"));
        } catch (OWLOntologyCreationException e) {
            LOG.error(e.getMessage(), e);
            throw new OWL2JavaException("Unable to load ontology " + owlOntologyName, e);
        }
    }

    private void addAxiomToContext(final ContextDefinition ctx, final OWLAxiom axiom) {
        for (final OWLEntity e : axiom.getSignature()) {
            if (e.isOWLClass() && !skipped.contains(e.getIRI())) {
                ctx.classes.add(e.asOWLClass());
            }
            if (e.isOWLObjectProperty() && !skipped.contains(e.getIRI())) {
                ctx.objectProperties.add(e.asOWLObjectProperty());
            }
            if (e.isOWLDataProperty() && !skipped.contains(e.getIRI())) {
                ctx.dataProperties.add(e.asOWLDataProperty());
            }
            if (e.isOWLAnnotationProperty() && !skipped.contains(e.getIRI())) {
                ctx.annotationProperties.add(e.asOWLAnnotationProperty());
            }
            if (e.isOWLNamedIndividual() && !skipped.contains(e.getIRI())) {
                ctx.individuals.add(e.asOWLNamedIndividual());
            }
        }
        ctx.axioms.add(axiom);
    }

    public void setOntology(final String owlOntologyName,
                            final String mappingFile, boolean includeImports) {
        ontology = getWholeOntology(owlOntologyName, mappingFile);

//        this.imports = ontology.getOWLOntologyManager().getOntologies();

        LOG.info("Parsing integrity constraints");

        for (final OWLAxiom a : ontology.getAxioms()) {
            addAxiomToContext(DEFAULT_CONTEXT, a);
            for (final String icContextName : getContexts(a)) {
                ContextDefinition ctx = getContextDefinition(icContextName);
                addAxiomToContext(ctx, a);
            }
        }

        DEFAULT_CONTEXT.parse();
        for (final ContextDefinition ctx : contexts.values()) {
            ctx.parse();
        }

        LOG.info("Integrity constraints successfully parsed.");
    }

    private ContextDefinition getContextDefinition(String icContextName) {
        return contexts.computeIfAbsent(icContextName, ContextDefinition::new);
    }

    private List<String> getContexts(final OWLAxiom a) {
        final List<String> contexts = new ArrayList<>();
        for (final OWLAnnotation p : a.getAnnotations()) {
            LOG.info("Processing annotation : " + p);
            if (!p.getProperty().getIRI().toString().equals(Constants.P_IS_INTEGRITY_CONSTRAINT_FOR)) {
                continue;
            }
            p.getValue().accept(v);
            final String icContextName = v.getName();
            LOG.info("CONTEXT:" + icContextName);
            if (icContextName == null) {
                continue;
            }
            LOG.debug("Found IC {} for context {}", a, icContextName);
            contexts.add(icContextName);
        }
        return contexts;
    }

    private void verifyContextExistence(String context) {
        if (!contexts.containsKey(context)) {
            throw new IllegalArgumentException(
                    "Context " + context + " not found. Existing contexts: " + listContexts());
        }
    }

    public void transform(String context, String pkg, String targetDir, boolean withOWLAPI) {
        LOG.info("Transforming context ...");
        if (context == null) {
            LOG.info(" - for all axioms");
        } else {
            LOG.info(" - for context '{}'.", context);
            verifyContextExistence(context);
        }

        ContextDefinition def = context == null ? DEFAULT_CONTEXT : contexts.get(context);
        new JavaTransformer().generateModel(ontology, def, pkg, targetDir, withOWLAPI);
        LOG.info("Transformation SUCCESSFUL.");
    }

    /**
     * Generates only vocabulary of the loaded ontology.
     *
     * @param context    Integrity constraints context, if null is supplied, the whole ontology is interpreted as integrity constraints.
     * @param targetDir  Directory into which the vocabulary file will be generated
     * @param pkg        Package
     * @param withOWLAPI Whether OWLAPI-based IRIs of the generated vocabulary items should be created as well
     */
    public void generateVocabulary(String context, String pkg, String targetDir, boolean withOWLAPI) {
        LOG.info("Generating vocabulary ...");
        if (context == null) {
            LOG.info(" - for all axioms");
        } else {
            LOG.info(" - for context '{}'.", context);
            verifyContextExistence(context);
        }
        ContextDefinition def = (context == null) ? DEFAULT_CONTEXT : contexts.get(context);
        new JavaTransformer().
                                     generateVocabulary(ontology, def, pkg, targetDir, withOWLAPI);
    }

    private class ValidContextAnnotationValueVisitor implements OWLAnnotationValueVisitor {
        private String name = null;

        String getName() {
            return name;
        }

        public void visit(IRI iri) {
        }

        public void visit(OWLAnonymousIndividual individual) {
        }

        public void visit(OWLLiteral literal) {
            name = literal.getLiteral();
        }
    }
}
