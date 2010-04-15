package cz.cvut.kbss.owlpersistence.model.ic;

public interface IntegrityConstraint {

	public void accept(IntegrityConstraintVisitor visitor);

}
