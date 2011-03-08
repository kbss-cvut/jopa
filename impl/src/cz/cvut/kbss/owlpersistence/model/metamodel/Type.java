package cz.cvut.kbss.owlpersistence.model.metamodel;

import cz.cvut.kbss.owlpersistence.UnusedJPA;

/**
 * Instances of the type Type represent persistent object or attribute types.
 * 
 * @param <X>
 *            The type of the represented object or attribute
 */
public interface Type<X> {
	public static enum PersistenceType {
		ENTITY,

		@Deprecated
		@UnusedJPA
		/**
		 * Embeddable classes are logical groupings of state in Java that is "flattened" in RDB. Such mapping seems useless, or at lest of low priority, in ontologies. 
		 */
		EMBEDDABLE,

		@Deprecated
		@UnusedJPA
		/**
		 * Mapped superclasses help building entities with common state definition. They are not entities. For ontologies this requirement seems useless as creating hierarchies is a natural operation.
		 */
		MAPPED_SUPERCLASS,

		BASIC,
	}

	/**
	 * Return the persistence type.
	 * 
	 * @return persistence type
	 */
	PersistenceType getPersistenceType();

	/**
	 * Return the represented Java type.
	 * 
	 * @return Java type
	 */
	Class<X> getJavaType();
}
