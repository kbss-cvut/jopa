package cz.cvut.kbss.owlpersistence.model.metamodel;

public interface IdentifierVisitor {

	public void visit(IRIIdentifier i);

	public void visit(KeyIdentifier i);

	public void visit(SameAsIdentifier i);
}
