package cz.cvut.kbss.owlpersistence;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import cz.cvut.kbss.owlpersistence.owlapi.SequencesVocabulary;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface OWLSequence {

	/**
	 * Defines the type of the sequence.
	 */
	OWLSequenceType type() default OWLSequenceType.referenced;

	/**
	 * URI of the class that represents the 'OWLList' concept.
	 * 
	 * Relevant only for REFERENCED type.
	 */
	String ObjectPropertyHasSequence();

	/**
	 * URI of the class that represents the 'OWLList' concept.
	 * 
	 * Relevant only for REFERENCED type.
	 */
	String ClassOWLListURI() default SequencesVocabulary.s_c_OWLList;

	/**
	 * URI of the object property that represents the 'hasContents' role.
	 * 
	 * Relevant only for REFERENCED type.
	 */
	String ObjectPropertyHasContentsURI() default SequencesVocabulary.s_p_hasContents;

	/**
	 * URI of the object property that represents the 'hasNext' role.
	 */
	String ObjectPropertyHasNextURI() default SequencesVocabulary.s_p_hasNext;

	CascadeType[] cascadeType() default { CascadeType.ALL };
}
