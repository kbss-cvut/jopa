/**
 * Copyright (C) 2011 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package cz.cvut.kbss.owlpersistence.model.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import cz.cvut.kbss.owlpersistence.model.SequencesVocabulary;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Sequence {

	/**
	 * Defines the type of the sequence.
	 */
	SequenceType type() default SequenceType.referenced;

	/**
	 * URI of the class that represents the 'OWLList' concept.
	 * 
	 * Relevant only for REFERENCED type.
	 */
	String ClassOWLListIRI() default SequencesVocabulary.s_c_OWLList;
	
	/**
	 * URI of the object property that represents the 'hasContents' role.
	 * 
	 * Relevant only for REFERENCED type.
	 */
	String ObjectPropertyHasContentsIRI() default SequencesVocabulary.s_p_hasContents;

	/**
	 * URI of the object property that represents the 'hasNext' role.
	 */
	String ObjectPropertyHasNextIRI() default SequencesVocabulary.s_p_hasNext;
}
