package cz.cvut.kbss.owlpersistence.model.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface ParticipationConstraint {

	/**
	 * IRI of the object property
	 * 
	 * @return IRI of the object property
	 */
	String owlClassIRI() default "http://www.w3.org/2002/07/owl#Thing";

	/**
	 * IRI of the object property
	 * 
	 * @return IRI of the object property
	 */
	String owlPropertyIRI();

	/**
	 * IRI of the property filler
	 * 
	 * @return IRI of the property filler
	 */
	String owlObjectIRI();

	/**
	 * cardinality
	 * 
	 * @return minimal cardinality
	 */
	int min() default 0;

	/**
	 * cardinality
	 * 
	 * negative integer means positive infinity
	 * 
	 * @return maximal cardinality
	 */
	int max() default -1;
}
