package cz.cvut.kbss.owlpersistence.model.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface DomainOf {
	/**
	 * IRI of the object/data property
	 * 
	 * @return IRI of the object/data property
	 */
	String owlPropertyIRI();
}
