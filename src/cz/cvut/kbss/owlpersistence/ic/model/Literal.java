package cz.cvut.kbss.owlpersistence.ic.model;

public interface Literal extends AnnotationValue {

	public String getLang();

	public String getLexicalForm();

	public IRI getDatatype();

}
