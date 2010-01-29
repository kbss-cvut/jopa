package cz.cvut.kbss.owlpersistence.model.metamodel;

import java.lang.reflect.Field;

public interface Identifier {
	public Field getJavaField();

	public void accept(IdentifierVisitor i);
}
