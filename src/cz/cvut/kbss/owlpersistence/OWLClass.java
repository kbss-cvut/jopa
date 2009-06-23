package cz.cvut.kbss.owlpersistence;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface OWLClass {
	
	/**
	 * URI of the resource
	 * 
	 * @return URI of the referenced class
	 */
	String uri();
	
	boolean mutable() default true;	
}
