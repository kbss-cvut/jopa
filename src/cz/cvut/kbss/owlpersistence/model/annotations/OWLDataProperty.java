package cz.cvut.kbss.owlpersistence.model.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface OWLDataProperty {

	/**
	 * IRI of the data property
	 * 
	 * @return IRI of the data property
	 */
	String iri();

	/**
	 * IRI of the property filler
	 * 
	 * @return IRI of the property filler
	 */
	String fillerIri() default "http://www.w3.org/2000/01/rdf-schema#Literal";

	/**
	 * minimal cardinality
	 * 
	 * @return minimal cardinality
	 */
	int min() default 0;

	/**
	 * maximal cardinality
	 * 
	 * @return maximal cardinality
	 */
	int max() default Integer.MAX_VALUE;
}
