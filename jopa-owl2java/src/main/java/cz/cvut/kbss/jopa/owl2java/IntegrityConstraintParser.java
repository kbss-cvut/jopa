/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.owl2java;

import cz.cvut.kbss.jopa.owl2java.exception.UnsupportedICException;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Map;

public class IntegrityConstraintParser implements OWLAxiomVisitor {

    private static final Logger LOG = LoggerFactory.getLogger(OWL2JavaTransformer.class);

    private IntegrityConstraintSet integrityConstraintSet = new IntegrityConstraintSet();

    private Map<OWLObjectProperty, OWLClass> opRanges = new HashMap<>();
    private Map<OWLDataProperty, OWLDatatype> dpRanges = new HashMap<>();

    @Override
    public void visit(OWLAnnotationPropertyRangeAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLAnnotationPropertyDomainAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLSubAnnotationPropertyOfAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLAnnotationAssertionAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(SWRLRule axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLDatatypeDefinitionAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLHasKeyAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLInverseObjectPropertiesAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLSubPropertyChainOfAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLSameIndividualAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLInverseFunctionalObjectPropertyAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLSubDataPropertyOfAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLIrreflexiveObjectPropertyAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLTransitiveObjectPropertyAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLDataPropertyAssertionAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLEquivalentClassesAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLClassAssertionAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLEquivalentDataPropertiesAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLFunctionalDataPropertyAxiom axiom) {
        // ic.addAll(processParticipationConstraint(f.getOWLThing(), f
        // .getOWLDataMaxCardinality(1, axiom.getProperty())));

        // processParticipationConstraint(f.getOWLThing(), f
        // .getOWLDataMaxCardinality(1, axiom.getProperty()));
        notSupported(axiom);
    }

    @Override
    public void visit(OWLDataPropertyRangeAxiom axiom) {
        try {
            OWLDataProperty op = Utils.ensureDataProperty(axiom.getProperty());
            OWLDatatype clz = Utils.ensureDatatype(axiom.getRange());

            dpRanges.put(op, clz);
        } catch (UnsupportedICException e) {
            notSupported(axiom);
        }
    }

    @Override
    public void visit(OWLSymmetricObjectPropertyAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLDisjointUnionAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLSubObjectPropertyOfAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLFunctionalObjectPropertyAxiom axiom) {
        // ic.addAll(processParticipationConstraint(f.getOWLThing(), f
        // .getOWLObjectMaxCardinality(1, axiom.getProperty())));

        // processParticipationConstraint(f.getOWLThing(), f
        // .getOWLObjectMaxCardinality(1, axiom.getProperty()));
        notSupported(axiom);
    }

    @Override
    public void visit(OWLObjectPropertyAssertionAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLObjectPropertyRangeAxiom axiom) {
        try {
            OWLObjectProperty op = Utils.ensureObjectProperty(axiom.getProperty());
            OWLClass clz = Utils.ensureClass(axiom.getRange());

            opRanges.put(op, clz);
//            processSubClassConstraintCandidate(f.getOWLThing(),
//                OWLManager.getOWLDataFactory().getOWLObjectAllValuesFrom(op, clz));
        } catch (UnsupportedICException e) {
            notSupported(axiom);
        }
    }

    @Override
    public void visit(OWLDisjointObjectPropertiesAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLDisjointDataPropertiesAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLDifferentIndividualsAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLNegativeDataPropertyAssertionAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLEquivalentObjectPropertiesAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLObjectPropertyDomainAxiom axiom) {
        // OWLObjectProperty op = ensureObjectProperty(axiom.getProperty());
        // OWLClass clz = ensureClass(axiom.getDomain());
        // Set<ObjectDomainConstraint> c = odConstraints.get(op);
        // if (c == null) {
        // c = new HashSet<ObjectDomainConstraint>();
        // odConstraints.put(op, c);
        // }
        //
        // c.add(IntegrityConstraintFactoryImpl
        // .ObjectPropertyDomainConstraint(op, clz));
        notSupported(axiom);
    }

    @Override
    public void visit(OWLDataPropertyDomainAxiom axiom) {
        // OWLDataProperty op = ensureDataProperty(axiom.getProperty());
        // OWLClass clz = ensureClass(axiom.getDomain());
        // Set<DataDomainConstraint> c = ddConstraints.get(op);
        // if (c == null) {
        // c = new HashSet<DataDomainConstraint>();
        // ddConstraints.put(op, c);
        // }
        //
        // c.add(IntegrityConstraintFactoryImpl.DataPropertyDomainConstraint(op,
        // clz));
        notSupported(axiom);
    }

    @Override
    public void visit(OWLDisjointClassesAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLReflexiveObjectPropertyAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLAsymmetricObjectPropertyAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLNegativeObjectPropertyAssertionAxiom axiom) {
        notSupported(axiom);
    }

    @Override
    public void visit(OWLSubClassOfAxiom axiom) {
        try {
            if (!axiom.getSubClass().isAnonymous()) {
                processSubClassConstraintCandidate(axiom.getSubClass().asOWLClass(),
                        axiom.getSuperClass());
            } else {
                notSupported(axiom);
            }
        } catch (UnsupportedICException e) {
            notSupported(axiom);
        }
    }

    @Override
    public void visit(final OWLDeclarationAxiom axiom) {
        notSupported(axiom);
//        axiom.getEntity().accept(new OWLEntityVisitor() {
//
//            public void visit(OWLAnnotationProperty property) {
//                ctx.annotationProperties.add(property);
//            }
//
//            public void visit(OWLDatatype datatype) {
//                notSupported(axiom);
//            }
//
//            public void visit(OWLNamedIndividual individual) {
//                notSupported(axiom);
//            }
//
//            public void visit(OWLDataProperty property) {
//                ctx.dataProperties.add(property);
//            }
//
//            public void visit(OWLObjectProperty property) {
//                ctx.objectProperties.add(property);
//            }
//
//            public void visit(OWLClass cls) {
//                ctx.classes.add(cls);
//            }
//        });
    }

    private static void notSupported(final OWLObject o) {
        LOG.info("Ignoring Unsupported Axiom : {}", o);
    }

    private void processSubClassConstraintCandidate(final OWLClass subjClass,
                                                    final OWLClassExpression superClass) {
        final IntegrityConstraintClassParser icp = new IntegrityConstraintClassParser(subjClass);
        superClass.accept(icp);

        for (Map.Entry<OWLObjectProperty, OWLClass> entry : opRanges.entrySet()) {
            final OWLObjectProperty property = entry.getKey();
            if (superClass.signature().anyMatch(e -> e.equals(property))) {
                OWLManager.getOWLDataFactory().getOWLObjectAllValuesFrom(property, entry.getValue()).accept(icp);
            }
        }
        for (Map.Entry<OWLDataProperty, OWLDatatype> entry : dpRanges.entrySet()) {
            final OWLDataProperty property = entry.getKey();
            if (superClass.signature().anyMatch(e -> e.equals(property))) {
                OWLManager.getOWLDataFactory().getOWLDataAllValuesFrom(property, entry.getValue()).accept(icp);
            }
        }
        icp.getIntegrityConstraints().forEach(ic -> integrityConstraintSet.addIntegrityConstraint(ic));
    }

    public IntegrityConstraintSet getClassIntegrityConstraintSet() {
        return integrityConstraintSet;
    }
}
