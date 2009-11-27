package cz.cvut.kbss.owlpersistence.ic.internalmodel;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;

public class OWLPersistenceAnnotationFactory {

	public static ClassParticipationConstraint classParticipationConstraint(
			final OWLClass c, final OWLObjectProperty op, final OWLClass o,
			final Integer minRequired, final Integer maxPossible) {
		return new ClassParticipationConstraint() {

			@Override
			public OWLClass getSubject() {
				return c;
			}

			@Override
			public Integer getMinRequired() {
				return minRequired;
			}

			@Override
			public Integer getMaxPossible() {
				return maxPossible;
			}

			@Override
			public OWLObjectProperty getPredicate() {
				return op;
			}

			@Override
			public OWLClass getObject() {
				return o;
			}
		};
	}

	public static DatatypeParticipationConstraint datatypeParticipationConstraint(
			final OWLClass c, final OWLDataProperty op, final OWLDatatype dt,
			final Integer minRequired, final Integer maxPossible) {
		return new DatatypeParticipationConstraint() {

			@Override
			public OWLClass getSubject() {
				return c;
			}

			@Override
			public Integer getMinRequired() {
				return minRequired;
			}

			@Override
			public Integer getMaxPossible() {
				return maxPossible;
			}

			@Override
			public OWLDataProperty getPredicate() {
				return op;
			}

			@Override
			public OWLDatatype getObject() {
				return dt;
			}
		};

	}

	public static LiteralParticipationConstraint literalParticipationConstraint(
			final OWLClass c, final OWLDataProperty op, final OWLLiteral l) {
		return new LiteralParticipationConstraint() {

			@Override
			public OWLClass getSubject() {
				return c;
			}

			@Override
			public OWLDataProperty getPredicate() {
				return op;
			}

			@Override
			public OWLLiteral getObject() {
				return l;
			}
		};

	}

	public static NamedIndividualParticipationConstraint namedIndividualParticipationConstraint(
			final OWLClass c, final OWLObjectProperty op,
			final OWLNamedIndividual l) {
		return new NamedIndividualParticipationConstraint() {

			@Override
			public OWLClass getSubject() {
				return c;
			}

			@Override
			public OWLObjectProperty getPredicate() {
				return op;
			}

			@Override
			public OWLNamedIndividual getObject() {
				return l;
			}
		};

	}
}
