package cz.cvut.kbss.owlpersistence.ic.generator;

import java.util.HashSet;
import java.util.Set;

import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLClassExpressionVisitor;
import org.semanticweb.owlapi.model.OWLDataAllValuesFrom;
import org.semanticweb.owlapi.model.OWLDataExactCardinality;
import org.semanticweb.owlapi.model.OWLDataHasValue;
import org.semanticweb.owlapi.model.OWLDataMaxCardinality;
import org.semanticweb.owlapi.model.OWLDataMinCardinality;
import org.semanticweb.owlapi.model.OWLDataPropertyExpression;
import org.semanticweb.owlapi.model.OWLDataRange;
import org.semanticweb.owlapi.model.OWLDataSomeValuesFrom;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLObjectAllValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectComplementOf;
import org.semanticweb.owlapi.model.OWLObjectExactCardinality;
import org.semanticweb.owlapi.model.OWLObjectHasSelf;
import org.semanticweb.owlapi.model.OWLObjectHasValue;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;
import org.semanticweb.owlapi.model.OWLObjectMaxCardinality;
import org.semanticweb.owlapi.model.OWLObjectMinCardinality;
import org.semanticweb.owlapi.model.OWLObjectOneOf;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectUnionOf;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;

import cz.cvut.kbss.owlpersistence.ic.internalmodel.OWLPersistenceAnnotation;
import cz.cvut.kbss.owlpersistence.ic.internalmodel.OWLPersistenceAnnotationFactory;

public class IntegrityConstraintParserImpl implements IntegrityConstraintParser {

	@Override
	public Set<OWLPersistenceAnnotation> parse(OWLAxiom a, OWLOntology o)
			throws UnsupportedICException {
		if (!a.isOfType(AxiomType.SUBCLASS_OF)) {
			throw new UnsupportedICException("Unsupported axiom type : "
					+ a.getAxiomType());
		}

		final OWLSubClassOfAxiom ax = (OWLSubClassOfAxiom) a;

		if (ax.getSubClass().isAnonymous()) {
			throw new UnsupportedICException(
					"Class expressions not supported in subClassOf ICs."
							+ ax.getSubClass());
		}

		final OWLClass subjClass = ax.getSubClass().asOWLClass();

		try {
			return processSubClassOfAxiom(subjClass, ax.getSuperClass());
		} catch (RuntimeException e) {
			throw new UnsupportedICException(e.getMessage(), e);
		}
	}

	private void ensureDatatype(final OWLDataRange r)
			throws RuntimeException {
		if (!r.isDatatype()) {
			throw new RuntimeException("Data ranges not supported: " + r);
		}

		if (!r.asOWLDatatype().isBuiltIn()) {
			throw new RuntimeException(
					"Only built in datatypes are supported: " + r);
		}
	}

	private void ensureClass(final OWLClassExpression r)
			throws RuntimeException {
		if (r.isAnonymous()) {
			throw new RuntimeException("Class expressions not supported: " + r);
		}
	}

	private void ensureNamedIndividual(final OWLIndividual r)
			throws RuntimeException {
		if (r.isAnonymous()) {
			throw new RuntimeException("Anonymous individuals not supported: "
					+ r);
		}
	}

	private void ensureDataProperty(final OWLDataPropertyExpression e)
			throws RuntimeException {
		if (e.isAnonymous()) {
			throw new RuntimeException(
					"Data property expressions not supported: " + e);
		}
	}

	private void ensureObjectProperty(final OWLObjectPropertyExpression e)
			throws RuntimeException {
		if (e.isAnonymous()) {
			throw new RuntimeException(
					"Object property expressions not supported: " + e);
		}
	}

	private void ensureCardinality(int c) {
		if (c != 1) {
			throw new RuntimeException(
					"Cardinality constraints != 1 not supported : " + c);
		}
	}

	private Set<OWLPersistenceAnnotation> processSubClassOfAxiom(
			final OWLClass subjClass, final OWLClassExpression superClass) {
		final Set<OWLPersistenceAnnotation> set = new HashSet<OWLPersistenceAnnotation>();

		superClass.accept(new OWLClassExpressionVisitor() {

			@Override
			public void visit(OWLDataMaxCardinality arg0) {
				ensureDatatype(arg0.getFiller());
				ensureDataProperty(arg0.getProperty());
				ensureCardinality(arg0.getCardinality());

				set.add(OWLPersistenceAnnotationFactory
						.datatypeParticipationConstraint(subjClass, arg0
								.getProperty().asOWLDataProperty(), arg0
								.getFiller().asOWLDatatype(), 0, 1));
			}

			@Override
			public void visit(OWLDataExactCardinality arg0) {
				ensureDatatype(arg0.getFiller());
				ensureDataProperty(arg0.getProperty());
				ensureCardinality(arg0.getCardinality());

				set.add(OWLPersistenceAnnotationFactory
						.datatypeParticipationConstraint(subjClass, arg0
								.getProperty().asOWLDataProperty(), arg0
								.getFiller().asOWLDatatype(), 1, 1));
			}

			@Override
			public void visit(OWLDataMinCardinality arg0) {
				ensureDatatype(arg0.getFiller());
				ensureDataProperty(arg0.getProperty());
				ensureCardinality(arg0.getCardinality());

				set.add(OWLPersistenceAnnotationFactory
						.datatypeParticipationConstraint(subjClass, arg0
								.getProperty().asOWLDataProperty(), arg0
								.getFiller().asOWLDatatype(), 1, null));
			}

			@Override
			public void visit(OWLDataHasValue arg0) {
				ensureDataProperty(arg0.getProperty());

				set.add(OWLPersistenceAnnotationFactory
						.literalParticipationConstraint(subjClass, arg0
								.getProperty().asOWLDataProperty(), arg0
								.getValue()));
			}

			@Override
			public void visit(OWLDataAllValuesFrom arg0) {
				throw new RuntimeException(
						"OWLDataAllValuesFrom expressions not supported yet.");
			}

			@Override
			public void visit(OWLDataSomeValuesFrom arg0) {
				ensureDatatype(arg0.getFiller());
				ensureDataProperty(arg0.getProperty());

				set.add(OWLPersistenceAnnotationFactory
						.datatypeParticipationConstraint(subjClass, arg0
								.getProperty().asOWLDataProperty(), arg0
								.getFiller().asOWLDatatype(), 1, null));
			}

			@Override
			public void visit(OWLObjectOneOf arg0) {
				// ENUM
				throw new RuntimeException(
						"OWLObjectOneOf expressions not supported yet.");
			}

			@Override
			public void visit(OWLObjectHasSelf arg0) {
				throw new RuntimeException(
						"OWLObjectHasSelf expressions not supported yet.");
			}

			@Override
			public void visit(OWLObjectMaxCardinality arg0) {
				ensureClass(arg0.getFiller());
				ensureObjectProperty(arg0.getProperty());
				ensureCardinality(arg0.getCardinality());

				set.add(OWLPersistenceAnnotationFactory
						.classParticipationConstraint(subjClass, arg0
								.getProperty().asOWLObjectProperty(), arg0
								.getFiller().asOWLClass(), 0, 1));
			}

			@Override
			public void visit(OWLObjectExactCardinality arg0) {
				ensureClass(arg0.getFiller());
				ensureObjectProperty(arg0.getProperty());
				ensureCardinality(arg0.getCardinality());

				set.add(OWLPersistenceAnnotationFactory
						.classParticipationConstraint(subjClass, arg0
								.getProperty().asOWLObjectProperty(), arg0
								.getFiller().asOWLClass(), 1, 1));
			}

			@Override
			public void visit(OWLObjectMinCardinality arg0) {
				ensureClass(arg0.getFiller());
				ensureObjectProperty(arg0.getProperty());
				ensureCardinality(arg0.getCardinality());

				set.add(OWLPersistenceAnnotationFactory
						.classParticipationConstraint(subjClass, arg0
								.getProperty().asOWLObjectProperty(), arg0
								.getFiller().asOWLClass(), 1, null));
			}

			@Override
			public void visit(OWLObjectHasValue arg0) {
				ensureObjectProperty(arg0.getProperty());
				ensureNamedIndividual(arg0.getValue());

				set.add(OWLPersistenceAnnotationFactory
						.namedIndividualParticipationConstraint(subjClass, arg0
								.getProperty().asOWLObjectProperty(), arg0
								.getValue().asNamedIndividual()));
			}

			@Override
			public void visit(OWLObjectAllValuesFrom arg0) {
				throw new RuntimeException(
						"OWLObjectAllValuesFrom expressions not supported yet.");
			}

			@Override
			public void visit(OWLObjectSomeValuesFrom arg0) {
				ensureClass(arg0.getFiller());
				ensureObjectProperty(arg0.getProperty());

				set.add(OWLPersistenceAnnotationFactory
						.classParticipationConstraint(subjClass, arg0
								.getProperty().asOWLObjectProperty(), arg0
								.getFiller().asOWLClass(), 1, null));
			}

			@Override
			public void visit(OWLObjectComplementOf arg0) {
				throw new RuntimeException(
						"OWLObjectComplementOf expressions not supported yet.");
			}

			@Override
			public void visit(OWLObjectUnionOf arg0) {
				throw new RuntimeException(
						"OWLObjectUnionOf expressions not supported yet.");
			}

			@Override
			public void visit(OWLObjectIntersectionOf arg0) {
				throw new RuntimeException(
						"OWLObjectIntersectionOf expressions not supported yet.");
			}

			@Override
			public void visit(OWLClass arg0) {
				throw new RuntimeException(
						"OWLClass expressions not supported yet.");
			}
		});
		return set;
	}
}
