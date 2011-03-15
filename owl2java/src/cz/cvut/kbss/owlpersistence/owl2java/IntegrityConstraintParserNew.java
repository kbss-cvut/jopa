package cz.cvut.kbss.owlpersistence.owl2java;

import java.util.HashSet;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationPropertyDomainAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationPropertyRangeAxiom;
import org.semanticweb.owlapi.model.OWLAsymmetricObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLAxiomVisitor;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLClassExpressionVisitor;
import org.semanticweb.owlapi.model.OWLDataAllValuesFrom;
import org.semanticweb.owlapi.model.OWLDataExactCardinality;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataHasValue;
import org.semanticweb.owlapi.model.OWLDataMaxCardinality;
import org.semanticweb.owlapi.model.OWLDataMinCardinality;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLDataPropertyDomainAxiom;
import org.semanticweb.owlapi.model.OWLDataPropertyExpression;
import org.semanticweb.owlapi.model.OWLDataPropertyRangeAxiom;
import org.semanticweb.owlapi.model.OWLDataRange;
import org.semanticweb.owlapi.model.OWLDataSomeValuesFrom;
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
import org.semanticweb.owlapi.model.OWLObjectAllValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectComplementOf;
import org.semanticweb.owlapi.model.OWLObjectExactCardinality;
import org.semanticweb.owlapi.model.OWLObjectHasSelf;
import org.semanticweb.owlapi.model.OWLObjectHasValue;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;
import org.semanticweb.owlapi.model.OWLObjectMaxCardinality;
import org.semanticweb.owlapi.model.OWLObjectMinCardinality;
import org.semanticweb.owlapi.model.OWLObjectOneOf;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLObjectPropertyDomainAxiom;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLObjectPropertyRangeAxiom;
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectUnionOf;
import org.semanticweb.owlapi.model.OWLOntology;
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
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import cz.cvut.kbss.owlpersistence.model.ic.IntegrityConstraint;
import cz.cvut.kbss.owlpersistence.model.ic.IntegrityConstraintFactory;

public class IntegrityConstraintParserNew implements IntegrityConstraintParser {

	private OWLDataFactory f;

	private void notSupported(final OWLObject o) {
		System.out.println("Ignoring Unsupported Constraint: " + o);
	}

	
	public Set<IntegrityConstraint> parse(OWLAxiom a, OWLReasoner r,
			OWLOntology o) throws UnsupportedICException {
		this.f = o.getOWLOntologyManager().getOWLDataFactory();

		final Set<IntegrityConstraint> ic = new HashSet<IntegrityConstraint>();

		a.accept(new OWLAxiomVisitor() {

			
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
				ic.addAll(processParticipationConstraint(f.getOWLThing(), f
						.getOWLDataMaxCardinality(1, axiom.getProperty())));
			}

			
			public void visit(OWLDataPropertyRangeAxiom axiom) {
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
				ic.addAll(processParticipationConstraint(f.getOWLThing(), f
						.getOWLObjectMaxCardinality(1, axiom.getProperty())));

			}

			
			public void visit(OWLObjectPropertyAssertionAxiom axiom) {
				notSupported(axiom);
			}

			
			public void visit(OWLObjectPropertyRangeAxiom axiom) {
				ic.addAll(processParticipationConstraint(f.getOWLThing(), f
						.getOWLObjectMaxCardinality(1, axiom.getProperty())));
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
				notSupported(axiom);
			}

			
			public void visit(OWLDataPropertyDomainAxiom axiom) {
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
				if (axiom.getSubClass().isAnonymous()) {
					notSupported(axiom);
				}
				ic.addAll(processParticipationConstraint(axiom.getSubClass()
						.asOWLClass(), axiom.getSuperClass()));
			}

			
			public void visit(OWLDeclarationAxiom axiom) {
				notSupported(axiom);
			}
		});

		return ic;
	}

	private OWLDatatype ensureDatatype(final OWLDataRange r)
			throws RuntimeException {
		if (!r.isDatatype()) {
			throw new RuntimeException("Data ranges not supported: " + r);
		}

		if (!r.asOWLDatatype().isBuiltIn()) {
			throw new RuntimeException(
					"Only built in datatypes are supported: " + r);
		}

		return r.asOWLDatatype();
	}

	private OWLClass ensureClass(final OWLClassExpression r) {
		if (!r.isAnonymous()) {
			return r.asOWLClass();
		}
		throw new RuntimeException("Only named classes are supported: " + r);
	}

	// private void ensureNamedIndividual(final OWLIndividual r)
	// throws RuntimeException {
	// if (r.isAnonymous()) {
	// throw new RuntimeException("Anonymous individuals not supported: "
	// + r);
	// }
	// }

	private OWLDataProperty ensureDataProperty(final OWLDataPropertyExpression e)
			throws RuntimeException {
		if (e.isAnonymous()) {
			throw new RuntimeException(
					"Data property expressions not supported: " + e);
		}

		return e.asOWLDataProperty();
	}

	private OWLObjectProperty ensureObjectProperty(
			final OWLObjectPropertyExpression e) throws RuntimeException {
		if (e.isAnonymous()) {
			throw new RuntimeException(
					"Object property expressions not supported: " + e);
		}

		return e.asOWLObjectProperty();
	}

	private Set<IntegrityConstraint> processParticipationConstraint(
			final OWLClass subjClass, final OWLClassExpression superClass) {
		final Set<IntegrityConstraint> set = new HashSet<IntegrityConstraint>();

		final OWLClassExpressionVisitor v = new OWLClassExpressionVisitor() {

			
			public void visit(OWLDataMaxCardinality arg0) {
				final OWLDatatype dt = ensureDatatype(arg0.getFiller());
				final OWLDataProperty dp = ensureDataProperty(arg0
						.getProperty());

				set.add(IntegrityConstraintFactory
						.MaxDataParticipationConstraint(subjClass, dp, dt, arg0
								.getCardinality()));
			}

			
			public void visit(OWLDataExactCardinality arg0) {
				final OWLDatatype dt = ensureDatatype(arg0.getFiller());
				final OWLDataProperty dp = ensureDataProperty(arg0
						.getProperty());

				set.add(IntegrityConstraintFactory.DataParticipationConstraint(
						subjClass, dp, dt, arg0.getCardinality(), arg0
								.getCardinality()));
			}

			// private void processDataCardinality(final int minCardinality,
			// final int maxCardinality, OWLDataProperty p,
			// final OWLDatatype c) {
			//
			// final Set<OWLDatatype> validRanges = new HashSet<OWLDatatype>();
			//
			// for (final OWLDatatype cc : o.getDatatypesInSignature()) {
			// if (r.isEntailed(f.getOWLDataPropertyRangeAxiom(p, cc))) {
			// validRanges.add(cc);
			// }
			// }
			//
			// if (validRanges.isEmpty()) {
			// validRanges.add(c);
			// }
			//
			// // infer cardinality
			// for (final OWLDatatype cc : validRanges) {
			// if ((maxCardinality > 1)
			// && r.isEntailed(f.getOWLSubClassOfAxiom(subjClass,
			// f.getOWLDataMaxCardinality(1, p
			// .asOWLDataProperty(), cc)))
			// || r.isEntailed(f
			// .getOWLFunctionalDataPropertyAxiom(p
			// .asOWLDataProperty()))) {
			//
			// set.add(IntegrityConstraintFactory
			// .datatypeParticipationConstraint(subjClass, p,
			// cc, minCardinality, 1));
			// } else {
			// // must be consistent
			//
			// set.add(IntegrityConstraintFactory
			// .datatypeParticipationConstraint(subjClass, p,
			// cc, minCardinality, maxCardinality));
			// }
			// }
			// }

			
			public void visit(OWLDataMinCardinality arg0) {
				final OWLDatatype dt = ensureDatatype(arg0.getFiller());
				final OWLDataProperty dp = ensureDataProperty(arg0
						.getProperty());

				set.add(IntegrityConstraintFactory
						.MinDataParticipationConstraint(subjClass, dp, dt, arg0
								.getCardinality()));
			}

			
			public void visit(OWLDataHasValue arg0) {
				notSupported(arg0);
				// ensureDataProperty(arg0.getProperty());
				//
				// set.add(IntegrityConstraintFactory
				// .datatypeParticipationConstraint(subjClass, arg0
				// .getProperty().asOWLDataProperty(), f
				// .getOWLDataOneOf(arg0.getValue()), 1, 1));
			}

			
			public void visit(OWLDataAllValuesFrom arg0) {
				notSupported(arg0);
			}

			
			public void visit(OWLDataSomeValuesFrom arg0) {
				final OWLDatatype dt = ensureDatatype(arg0.getFiller());
				final OWLDataProperty dp = ensureDataProperty(arg0
						.getProperty());

				set.add(IntegrityConstraintFactory
						.MinDataParticipationConstraint(subjClass, dp, dt, 1));
			}

			
			public void visit(OWLObjectOneOf arg0) {
				notSupported(arg0);
			}

			
			public void visit(OWLObjectHasSelf arg0) {
				notSupported(arg0);
			}

			
			public void visit(OWLObjectMaxCardinality arg0) {
				OWLClass c = ensureClass(arg0.getFiller());
				OWLObjectProperty p = ensureObjectProperty(arg0.getProperty());

				set.add(IntegrityConstraintFactory
						.MaxObjectParticipationConstraint(subjClass, p, c, arg0
								.getCardinality()));
			}

			
			public void visit(OWLObjectExactCardinality arg0) {
				OWLClass c = ensureClass(arg0.getFiller());
				OWLObjectProperty p = ensureObjectProperty(arg0.getProperty());

				set.add(IntegrityConstraintFactory
						.ObjectParticipationConstraint(subjClass, p, c, arg0
								.getCardinality(), arg0.getCardinality()));
			}

			// private void processObjectMinCardinality(final int cardinality,
			// OWLObjectProperty p, final OWLClass c) {
			//
			// // infer range
			// final NodeSet<OWLClass> nSet = r.getObjectPropertyRanges(p,
			// false);
			//
			// final Set<OWLClass> validRanges = new HashSet<OWLClass>();
			//
			// for (final OWLClass cc : nSet.getFlattened()) {
			// if (r.isEntailed(f.getOWLSubClassOfAxiom(cc, c))) {
			// validRanges.add(cc);
			// }
			// }
			//
			// if (validRanges.isEmpty()) {
			// validRanges.add(c);
			// }
			//
			// // infer cardinality
			// for (final OWLClass cc : validRanges) {
			// if (r.isEntailed(f.getOWLSubClassOfAxiom(subjClass, f
			// .getOWLObjectMaxCardinality(cardinality, p
			// .asOWLObjectProperty(), cc)))
			// || r.isEntailed(f
			// .getOWLFunctionalObjectPropertyAxiom(p
			// .asOWLObjectProperty()))) {
			// set.add(IntegrityConstraintFactory
			// .classParticipationConstraint(subjClass, p, cc,
			// cardinality, cardinality));
			// } else {
			// // must be consistent
			// set.add(IntegrityConstraintFactory
			// .classParticipationConstraint(subjClass, p, cc,
			// cardinality, Integer.MAX_VALUE));
			// }
			// }
			// }

			
			public void visit(OWLObjectMinCardinality arg0) {
				OWLClass c = ensureClass(arg0.getFiller());
				OWLObjectProperty p = ensureObjectProperty(arg0.getProperty());

				set.add(IntegrityConstraintFactory
						.MinObjectParticipationConstraint(subjClass, p, c, arg0
								.getCardinality()));
			}

			
			public void visit(OWLObjectHasValue arg0) {
				notSupported(arg0);
				// ensureObjectProperty(arg0.getProperty());
				// ensureNamedIndividual(arg0.getValue());
				//
				// set.add(IntegrityConstraintFactory
				// .classParticipationConstraint(subjClass, arg0
				// .getProperty().asOWLObjectProperty(), f
				// .getOWLObjectOneOf(arg0.getValue()
				// .asOWLNamedIndividual()), 1, 1));
			}

			
			public void visit(OWLObjectAllValuesFrom arg0) {
				notSupported(arg0);
			}

			
			public void visit(OWLObjectSomeValuesFrom arg0) {
				OWLClass c = ensureClass(arg0.getFiller());
				OWLObjectProperty p = ensureObjectProperty(arg0.getProperty());

				set.add(IntegrityConstraintFactory
						.MinObjectParticipationConstraint(subjClass, p, c, 1));
			}

			
			public void visit(OWLObjectComplementOf arg0) {
				notSupported(arg0);
			}

			
			public void visit(OWLObjectUnionOf arg0) {
				notSupported(arg0);
			}

			
			public void visit(OWLObjectIntersectionOf arg0) {
				notSupported(arg0);
			}

			
			public void visit(OWLClass arg0) {
				notSupported(arg0);
			}
		};

		superClass.accept(v);

		return set;
	}
}
