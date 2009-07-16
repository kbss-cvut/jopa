package cz.cvut.kbss.owlpersistence;

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
	 * URI of the resource
	 * 
	 * @return URI of the referenced class
	 */
	String uri();

	CascadeType[] cascadeType() default {};

	FetchType fetchType() default FetchType.LAZY;
	
	boolean reasoningRequired() default true;

}
