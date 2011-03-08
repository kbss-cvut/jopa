package cz.cvut.kbss.owlpersistence.model.ic;

public interface IntegrityConstraintVisitor {

	void visit(final DataParticipationConstraint cpc);

	void visit(final ObjectParticipationConstraint cpc);

	void visit(final ObjectDomainConstraint cpc);

	void visit(final ObjectRangeConstraint cpc);

	void visit(final DataDomainConstraint cpc);

	void visit(final DataRangeConstraint cpc);
}
