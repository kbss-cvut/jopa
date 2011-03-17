package cz.cvut.kbss.owlpersistence.model.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface OWLAnnotationProperty {
	/**
	 * IRI of the annotation property
	 * 
	 * @return IRI of the annotation property
	 */
	String iri();

	/**
	 * Denotes a member that is inferred (true) using the OWL reasoner or just
	 * asserted (false).
	 */
	boolean inferred() default false;
}
