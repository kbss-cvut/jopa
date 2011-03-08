package cz.cvut.kbss.owlpersistence.model;

public interface Literal<T> extends AnnotationValue {

	public String getLang();

	public T getValue();
}
