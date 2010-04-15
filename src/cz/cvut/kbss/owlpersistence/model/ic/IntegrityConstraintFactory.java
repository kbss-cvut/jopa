package cz.cvut.kbss.owlpersistence.model.ic;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLObjectProperty;

public class IntegrityConstraintFactory {

	public static IntegrityConstraint MinDataParticipationConstraint(
			final OWLClass s, final OWLDataProperty p, final OWLDatatype o,
			final int card) {
		return new DataParticipationConstraintImpl(s, p, o, card, -1);
	}

	public static IntegrityConstraint MaxDataParticipationConstraint(
			final OWLClass s, final OWLDataProperty p, final OWLDatatype o,
			final int card) {
		return new DataParticipationConstraintImpl(s, p, o, Integer.MIN_VALUE,
				card);
	}

	public static IntegrityConstraint DataParticipationConstraint(
			final OWLClass s, final OWLDataProperty p, final OWLDatatype o,
			final int min, int max) {
		return new DataParticipationConstraintImpl(s, p, o, min, max);
	}

	public static IntegrityConstraint MinObjectParticipationConstraint(
			final OWLClass s, final OWLObjectProperty p, final OWLClass o,
			final int card) {
		return new ObjectParticipationConstraintImpl(s, p, o, card, -1);
	}

	public static IntegrityConstraint MaxObjectParticipationConstraint(
			final OWLClass s, final OWLObjectProperty p, final OWLClass o,
			final int card) {
		return new ObjectParticipationConstraintImpl(s, p, o,
				Integer.MIN_VALUE, card);
	}

	public static IntegrityConstraint ObjectParticipationConstraint(
			final OWLClass s, final OWLObjectProperty p, final OWLClass o,
			final int min, int max) {
		return new ObjectParticipationConstraintImpl(s, p, o, min, max);
	}
}
