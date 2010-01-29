package cz.cvut.kbss.owlpersistence.owlapi;

import java.lang.reflect.Field;

import cz.cvut.kbss.owlpersistence.model.metamodel.IRIIdentifier;
import cz.cvut.kbss.owlpersistence.model.metamodel.IdentifierVisitor;

public class IRIIdentifierImpl implements IRIIdentifier {

	final Field javaField;

	public IRIIdentifierImpl(final Field javaField) {
		this.javaField = javaField;
	}

	@Override
	public Field getJavaField() {
		return javaField;
	}

	@Override
	public void accept(IdentifierVisitor i) {
		i.visit(this);
	}
}
