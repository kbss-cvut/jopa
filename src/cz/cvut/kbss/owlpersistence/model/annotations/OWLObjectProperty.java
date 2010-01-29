package cz.cvut.kbss.owlpersistence.model.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface OWLObjectProperty {

	/**
	 * IRI of the object property
	 * 
	 * @return IRI of the object property
	 */
	String iri();

	/**
	 * IRI of the property filler
	 * 
	 * @return IRI of the property filler
	 */
	String fillerIri() default "http://www.w3.org/2002/07/owl#Thing";

	CascadeType[] cascadeType() default {};

	FetchType fetchType() default FetchType.LAZY;

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
