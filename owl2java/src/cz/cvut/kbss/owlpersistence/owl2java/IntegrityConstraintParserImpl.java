package cz.cvut.kbss.owlpersistence.owl2java;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
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
import org.semanticweb.owlapi.model.OWLEntityVisitor;
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentDataPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLFunctionalDataPropertyAxiom;
import org.semanticweb.owlapi.model.OWLFunctionalObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLHasKeyAxiom;
import org.semanticweb.owlapi.model.OWLInverseFunctionalObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLInverseObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLIrreflexiveObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
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

import cz.cvut.kbss.owlpersistence.model.ic.DataParticipationConstraint;
import cz.cvut.kbss.owlpersistence.model.ic.DataRangeConstraint;
import cz.cvut.kbss.owlpersistence.model.ic.IntegrityConstraint;
import cz.cvut.kbss.owlpersistence.model.ic.IntegrityConstraintFactory;
import cz.cvut.kbss.owlpersistence.model.ic.ObjectParticipationConstraint;
import cz.cvut.kbss.owlpersistence.model.ic.ObjectRangeConstraint;
import cz.cvut.kbss.owlpersistence.owl2java.OWL2JavaTransformer.Card;

public class IntegrityConstraintParserImpl implements OWLAxiomVisitor {

	private OWLDataFactory f;

	// private Map<OWLClass, OWLClass> superClasses = new HashMap<OWLClass,
	// OWLClass>();
	// private Map<OWLObjectProperty, Set<ObjectDomainConstraint>> odConstraints
	// = new HashMap<OWLObjectProperty, Set<ObjectDomainConstraint>>();
	// private Map<OWLDataProperty, Set<DataDomainConstraint>> ddConstraints =
	// new HashMap<OWLDataProperty, Set<DataDomainConstraint>>();
	// private Map<OWLObjectProperty, ObjectRangeConstraint> orConstraints = new
	// HashMap<OWLObjectProperty, ObjectRangeConstraint>();
	// private Map<OWLObjectProperty, DataRangeConstraint> drConstraints = new
	// HashMap<OWLObjectProperty, DataRangeConstraint>();
	// private Map<OWLObjectProperty, ObjectRangeConstraint> orConstraints = new
	// HashMap<OWLObjectProperty, ObjectRangeConstraint>();
	// private Map<OWLObjectProperty, DataRangeConstraint> drConstraints = new
	// HashMap<OWLObjectProperty, DataRangeConstraint>();
	private Map<OWLClass, Map<OWLObjectProperty, Set<IntegrityConstraint>>> opConstraints = new HashMap<OWLClass, Map<OWLObjectProperty, Set<IntegrityConstraint>>>();
	private Map<OWLClass, Map<OWLDataProperty, Set<IntegrityConstraint>>> dpConstraints = new HashMap<OWLClass, Map<OWLDataProperty, Set<IntegrityConstraint>>>();
	// private Map<OWLClass, Map<OWLDataProperty, Set<IntegrityConstraint>>>
	// dpConstraints = new HashMap<OWLClass, Map<OWLDataProperty,
	// Set<IntegrityConstraint>>>();

	// private Map<OWLObjectProperty,Set<OWLObjectProperty>> inverseProperties =
	// new HashMap<OWLObjectProperty, Set<OWLObjectProperty>>();

	private ContextDefinition ctx;

	public IntegrityConstraintParserImpl(final OWLDataFactory f,
			final ContextDefinition ctx) {
		this.ctx = ctx;
		this.f = f;
	}

	public void parse() {
		for (final OWLAxiom a : ctx.axioms) {
			a.accept(this);
		}
	}

	private void notSupported(String message, final OWLObject o) {
		System.out.println(message + " : " + o);
	}

	private void notSupported(final OWLObject o) {
		notSupported("Ignoring Unsupported Axiom", o);
	}

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
		// ic.addAll(processParticipationConstraint(f.getOWLThing(), f
		// .getOWLObjectMaxCardinality(1, axiom.getProperty())));

		OWLObjectProperty op = ensureObjectProperty(axiom.getProperty());
		OWLClass clz = ensureClass(axiom.getRange());
		// ObjectRangeConstraint c = orConstraints.get(op);
		// if (c == null) {
		// orConstraints.put(op, IntegrityConstraintFactory
		// .ObjectPropertyRangeConstraint(f.getOWLThing(), op, clz));
		// } else {
		// notSupported("Multiple ranges not supported", axiom);
		// }

		processParticipationConstraint(f.getOWLThing(), f
				.getOWLObjectAllValuesFrom(op, clz));
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
		// c.add(IntegrityConstraintFactory
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
		// c.add(IntegrityConstraintFactory.DataPropertyDomainConstraint(op,
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
		if (!axiom.getSubClass().isAnonymous()
				&& !axiom.getSuperClass().isAnonymous()) {
			// OWLClass supC = superClasses.get(axiom.getSubClass());
			// if (supC == null) {
			// superClasses.put(axiom.getSubClass().asOWLClass(), axiom
			// .getSuperClass().asOWLClass());
			// } else {
			// notSupported(
			// "Multiple inheritance not allowed in Java OO model",
			// axiom);
			// }
			notSupported(axiom);
		} else if (!axiom.getSubClass().isAnonymous()) {
			processParticipationConstraint(axiom.getSubClass().asOWLClass(),
					axiom.getSuperClass());
		} else {
			notSupported(axiom);
		}
	}

	
	public void visit(final OWLDeclarationAxiom axiom) {
		axiom.getEntity().accept(new OWLEntityVisitor() {

			
			public void visit(OWLAnnotationProperty property) {
				notSupported(axiom);
			}

			
			public void visit(OWLDatatype datatype) {
				notSupported(axiom);
			}

			
			public void visit(OWLNamedIndividual individual) {
				notSupported(axiom);
			}

			
			public void visit(OWLDataProperty property) {
				ctx.dataProperties.add(property);
			}

			
			public void visit(OWLObjectProperty property) {
				ctx.objectProperties.add(property);
			}

			
			public void visit(OWLClass cls) {
				ctx.classes.add(cls);
			}
		});
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

	private void processParticipationConstraint(final OWLClass subjClass,
			final OWLClassExpression superClass) {
		Map<OWLObjectProperty, Set<IntegrityConstraint>> setOP2 = opConstraints
				.get(subjClass);
		if (setOP2 == null) {
			setOP2 = new HashMap<OWLObjectProperty, Set<IntegrityConstraint>>();
			opConstraints.put(subjClass, setOP2);
		}
		Map<OWLDataProperty, Set<IntegrityConstraint>> setDP2 = dpConstraints
				.get(subjClass);
		if (setDP2 == null) {
			setDP2 = new HashMap<OWLDataProperty, Set<IntegrityConstraint>>();
			dpConstraints.put(subjClass, setDP2);
		}

		// final Map<OWLObjectProperty, Set<ObjectParticipationConstraint>>
		// mapOP = setOP2;
		// final Map<OWLDataProperty, Set<DataParticipationConstraint>> mapDP =
		// setDP2;
		final Map<OWLObjectProperty, Set<IntegrityConstraint>> mapOP = setOP2;
		final Map<OWLDataProperty, Set<IntegrityConstraint>> mapDP = setDP2;

		final OWLClassExpressionVisitor v = new OWLClassExpressionVisitor() {

			
			public void visit(OWLDataMaxCardinality arg0) {
				final OWLDatatype dt = ensureDatatype(arg0.getFiller());
				final OWLDataProperty dp = ensureDataProperty(arg0
						.getProperty());

				Set<IntegrityConstraint> dpc = mapDP.get(dp);
				if (dpc == null) {
					dpc = new HashSet<IntegrityConstraint>();
					mapDP.put(dp, dpc);
				}

				dpc.add(IntegrityConstraintFactory
						.MaxDataParticipationConstraint(subjClass, dp, dt, arg0
								.getCardinality()));
			}

			
			public void visit(OWLDataExactCardinality arg0) {
				final OWLDatatype dt = ensureDatatype(arg0.getFiller());
				final OWLDataProperty dp = ensureDataProperty(arg0
						.getProperty());

				Set<IntegrityConstraint> dpc = mapDP.get(dp);
				if (dpc == null) {
					dpc = new HashSet<IntegrityConstraint>();
					mapDP.put(dp, dpc);
				}

				dpc.add(IntegrityConstraintFactory.DataParticipationConstraint(
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

				Set<IntegrityConstraint> dpc = mapDP.get(dp);
				if (dpc == null) {
					dpc = new HashSet<IntegrityConstraint>();
					mapDP.put(dp, dpc);
				}

				dpc.add(IntegrityConstraintFactory
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

				Set<IntegrityConstraint> dpc = mapDP.get(dp);
				if (dpc == null) {
					dpc = new HashSet<IntegrityConstraint>();
					mapDP.put(dp, dpc);
				}

				dpc.add(IntegrityConstraintFactory
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

				Set<IntegrityConstraint> opc = mapOP.get(p);
				if (opc == null) {
					opc = new HashSet<IntegrityConstraint>();
					mapOP.put(p, opc);
				}

				opc.add(IntegrityConstraintFactory
						.MaxObjectParticipationConstraint(subjClass, p, c, arg0
								.getCardinality()));
			}

			
			public void visit(OWLObjectExactCardinality arg0) {
				OWLClass c = ensureClass(arg0.getFiller());
				OWLObjectProperty p = ensureObjectProperty(arg0.getProperty());

				Set<IntegrityConstraint> opc = mapOP.get(p);
				if (opc == null) {
					opc = new HashSet<IntegrityConstraint>();
					mapOP.put(p, opc);
				}

				opc.add(IntegrityConstraintFactory
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

				Set<IntegrityConstraint> opc = mapOP.get(p);
				if (opc == null) {
					opc = new HashSet<IntegrityConstraint>();
					mapOP.put(p, opc);
				}

				opc.add(IntegrityConstraintFactory
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
				// System.out.println("Found ICX: " + c);
				OWLObjectProperty op = ensureObjectProperty(arg0.getProperty());
				OWLClass clz = ensureClass(arg0.getFiller());

				Set<IntegrityConstraint> opc = mapOP.get(op);
				if (opc == null) {
					opc = new HashSet<IntegrityConstraint>();
					mapOP.put(op, opc);
				}			
				opc.add(IntegrityConstraintFactory
						.ObjectPropertyRangeConstraint(subjClass, op, clz));
			}

			
			public void visit(OWLObjectSomeValuesFrom arg0) {
				OWLClass c = ensureClass(arg0.getFiller());
				OWLObjectProperty p = ensureObjectProperty(arg0.getProperty());

				Set<IntegrityConstraint> opc = mapOP.get(p);
				if (opc == null) {
					opc = new HashSet<IntegrityConstraint>();
					mapOP.put(p, opc);
				}

				opc.add(IntegrityConstraintFactory
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
	}

//	public OWLClass getSuperClass(final OWLClass subClass) {
//		OWLClass superClass = superClasses.get(subClass);
//		// if (superClass == null) {
//		// superClass = f.getOWLThing();
//		// }
//		return superClass;
//	}

	interface ClassObjectPropertyComputer {

		public Card getCard();

		public OWLClass getFiller();

		public Set<ObjectParticipationConstraint> getParticipationConstraints();
	}

	interface ClassDataPropertyComputer {

		public Card getCard();

		public OWLDatatype getFiller();

		public Set<DataParticipationConstraint> getParticipationConstraints();
	}

	public ClassObjectPropertyComputer getClassObjectPropertyComputer(
			final OWLClass clazz, final OWLObjectProperty prop, final OWLOntology merged) {
		final Set<IntegrityConstraint> ics = new HashSet<IntegrityConstraint>();

		final Map<OWLObjectProperty,Set<IntegrityConstraint>> constraints = opConstraints.get(clazz);

		if ( constraints != null && constraints.containsKey(prop)) {
			ics.addAll( constraints.get(prop) );
		}
		
		return new ClassObjectPropertyComputer() {
			
			
			public Card getCard() {
				if (ics.isEmpty()) {
					return Card.NO;
				}

				for (ObjectParticipationConstraint opc : getParticipationConstraints()) {
					if (!(clazz.equals(opc.getSubject())
							&& prop.getSubProperties(merged).contains(opc.getPredicate()))) {
						continue;
					}

					if (!getFiller().getSubClasses(merged).contains(opc.getObject())) {
						continue;
					}

					if (opc.getMax() == 1) {
						return Card.ONE;
					}

				}
				return Card.MULTIPLE;
			}

			
			public OWLClass getFiller() {
				for( final IntegrityConstraint ic : ics ) {
					if ( ic instanceof ObjectRangeConstraint ) {
						return ((ObjectRangeConstraint) ic).getRange();
					}
				}

				return f.getOWLThing();
			}

			public Set<ObjectParticipationConstraint> getParticipationConstraints() {
				Set<ObjectParticipationConstraint> set = new HashSet<ObjectParticipationConstraint>();
				for( final IntegrityConstraint ic : ics ) {
					if ( ic instanceof ObjectParticipationConstraint) {
						set.add((ObjectParticipationConstraint) ic);
					}
				}

				return set;
			}
		};

	}

	public ClassDataPropertyComputer getClassDataPropertyComputer(
			final OWLClass clazz, final OWLDataProperty prop, final OWLOntology merged) {
		final Set<IntegrityConstraint> ics = new HashSet<IntegrityConstraint>();

		final Map<OWLDataProperty,Set<IntegrityConstraint>> constraints = dpConstraints.get(clazz);
		if ( constraints != null && constraints.containsKey(prop)) {
			ics.addAll( constraints.get(prop) );
		}
		
		return new ClassDataPropertyComputer() {
			
			
			public Card getCard() {
				if (ics.isEmpty()) {
					return Card.NO;
				}

				for (DataParticipationConstraint opc : getParticipationConstraints()) {
					if (!(clazz.equals(opc.getSubject())
							&& prop.getSubProperties(merged).contains(opc.getPredicate()))) {
						continue;
					}

					if (!getFiller().equals(opc.getObject())) {
						continue;
					}

					if (opc.getMax() == 1) {
						return Card.ONE;
					}

				}
				return Card.MULTIPLE;
			}

			
			public OWLDatatype getFiller() {
				for( final IntegrityConstraint ic : ics ) {
					if ( ic instanceof DataRangeConstraint ) {
						return ((DataRangeConstraint) ic).getRange();
					}
				}

				return f.getRDFPlainLiteral(); //f.getTopDatatype();
			}

			public Set<DataParticipationConstraint> getParticipationConstraints() {
				Set<DataParticipationConstraint> set = new HashSet<DataParticipationConstraint>();
				for( final IntegrityConstraint ic : ics ) {
					if ( ic instanceof DataParticipationConstraint) {
						set.add((DataParticipationConstraint) ic);
					}
				}

				return set;
			}
		};

	}
}
