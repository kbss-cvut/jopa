/**
 * Copyright (C) 2016 Czech Technical University in Prague
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

import java.util.HashMap;
import java.util.Map;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationPropertyDomainAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationPropertyRangeAxiom;
import org.semanticweb.owlapi.model.OWLAsymmetricObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLAxiomVisitor;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLDataPropertyDomainAxiom;
import org.semanticweb.owlapi.model.OWLDataPropertyRangeAxiom;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLDatatypeDefinitionAxiom;
import org.semanticweb.owlapi.model.OWLDeclarationAxiom;
import org.semanticweb.owlapi.model.OWLDifferentIndividualsAxiom;
import org.semanticweb.owlapi.model.OWLDisjointClassesAxiom;
import org.semanticweb.owlapi.model.OWLDisjointDataPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLDisjointObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLDisjointUnionAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentDataPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLFunctionalDataPropertyAxiom;
import org.semanticweb.owlapi.model.OWLFunctionalObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLHasKeyAxiom;
import org.semanticweb.owlapi.model.OWLInverseFunctionalObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLInverseObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLIrreflexiveObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLNegativeDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLNegativeObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLObjectPropertyDomainAxiom;
import org.semanticweb.owlapi.model.OWLObjectPropertyRangeAxiom;
import org.semanticweb.owlapi.model.OWLReflexiveObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLSameIndividualAxiom;
import org.semanticweb.owlapi.model.OWLSubAnnotationPropertyOfAxiom;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;
import org.semanticweb.owlapi.model.OWLSubDataPropertyOfAxiom;
import org.semanticweb.owlapi.model.OWLSubObjectPropertyOfAxiom;
import org.semanticweb.owlapi.model.OWLSubPropertyChainOfAxiom;
import org.semanticweb.owlapi.model.OWLSymmetricObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLTransitiveObjectPropertyAxiom;
import org.semanticweb.owlapi.model.SWRLRule;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class IntegrityConstraintParser implements OWLAxiomVisitor {

    private static final Logger LOG = LoggerFactory.getLogger(OWL2JavaTransformer.class);

    private IntegrityConstraintSet integrityConstraintSet = new IntegrityConstraintSet();

    private Map<OWLObjectProperty, OWLClass> opRanges = new HashMap<>();
    private Map<OWLDataProperty, OWLDatatype> dpRanges = new HashMap<>();

    public void visit(OWLAnnotationPropertyRangeAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLAnnotationPropertyDomainAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLSubAnnotationPropertyOfAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLAnnotationAssertionAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(SWRLRule axiom) {
        notSupported(axiom);
    }

    public void visit(OWLDatatypeDefinitionAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLHasKeyAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLInverseObjectPropertiesAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLSubPropertyChainOfAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLSameIndividualAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLInverseFunctionalObjectPropertyAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLSubDataPropertyOfAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLIrreflexiveObjectPropertyAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLTransitiveObjectPropertyAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLDataPropertyAssertionAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLEquivalentClassesAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLClassAssertionAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLEquivalentDataPropertiesAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLFunctionalDataPropertyAxiom axiom) {
        // ic.addAll(processParticipationConstraint(f.getOWLThing(), f
        // .getOWLDataMaxCardinality(1, axiom.getProperty())));

        // processParticipationConstraint(f.getOWLThing(), f
        // .getOWLDataMaxCardinality(1, axiom.getProperty()));
        notSupported(axiom);
    }

    public void visit(OWLDataPropertyRangeAxiom axiom) {
        OWLDataProperty op = Utils.ensureDataProperty(axiom.getProperty());
        OWLDatatype clz = Utils.ensureDatatype(axiom.getRange());

        dpRanges.put(op, clz);
        notSupported(axiom);
    }

    public void visit(OWLSymmetricObjectPropertyAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLDisjointUnionAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLSubObjectPropertyOfAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLFunctionalObjectPropertyAxiom axiom) {
        // ic.addAll(processParticipationConstraint(f.getOWLThing(), f
        // .getOWLObjectMaxCardinality(1, axiom.getProperty())));

        // processParticipationConstraint(f.getOWLThing(), f
        // .getOWLObjectMaxCardinality(1, axiom.getProperty()));
        notSupported(axiom);
    }

    public void visit(OWLObjectPropertyAssertionAxiom axiom) {
        notSupported(axiom);
    }

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

    public void visit(OWLDisjointObjectPropertiesAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLDisjointDataPropertiesAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLDifferentIndividualsAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLNegativeDataPropertyAssertionAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLEquivalentObjectPropertiesAxiom axiom) {
        notSupported(axiom);
    }

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

    public void visit(OWLDisjointClassesAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLReflexiveObjectPropertyAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLAsymmetricObjectPropertyAxiom axiom) {
        notSupported(axiom);
    }

    public void visit(OWLNegativeObjectPropertyAssertionAxiom axiom) {
        notSupported(axiom);
    }

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

        for (final OWLObjectProperty property : opRanges.keySet()) {
            if (superClass.getSignature().contains(property)) {
                OWLManager.getOWLDataFactory().getOWLObjectAllValuesFrom(property, opRanges.get(property)).accept(icp);
            }
        }
        for (final OWLDataProperty property : dpRanges.keySet()) {
            if (superClass.getSignature().contains(property)) {
                OWLManager.getOWLDataFactory().getOWLDataAllValuesFrom(property, dpRanges.get(property)).accept(icp);
            }
        }

        icp.getIntegrityConstraints().forEach((ic) -> integrityConstraintSet.addIntegrityConstraint(ic));
    }

    public IntegrityConstraintSet getClassIntegrityConstraintSet() {
        return integrityConstraintSet;
    }
}