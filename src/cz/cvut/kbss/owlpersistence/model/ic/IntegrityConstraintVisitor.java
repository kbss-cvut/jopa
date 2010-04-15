package cz.cvut.kbss.owlpersistence.model.ic;

public interface IntegrityConstraintVisitor {

	void visit(final DataParticipationConstraint cpc);

	void visit(final ObjectParticipationConstraint cpc);
}
