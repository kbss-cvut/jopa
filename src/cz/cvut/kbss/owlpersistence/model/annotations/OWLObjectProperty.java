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

	CascadeType[] cascade() default {};

	FetchType fetch() default FetchType.LAZY;
}
