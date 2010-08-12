package cz.cvut.kbss.owlpersistence.model.metamodel;

import cz.cvut.kbss.owlpersistence.NonJPA;
import cz.cvut.kbss.owlpersistence.model.IRI;

/**
 * Instances of the type EntityType represent entity types.
 * 
 * @param <X>
 *            The represented entity type.
 */
public interface EntityType<X> extends IdentifiableType<X>, Bindable<X> {
	/**
	 * Return the entity name.
	 * 
	 * @return entity name
	 */
	String getName();

	@NonJPA
	IRI getIRI();

	@NonJPA
	Identifier getIdentifier();

	@NonJPA
	TypesSpecification<? super X, ?> getTypes();

	@NonJPA
	PropertiesSpecification<? super X, ?> getProperties();
}
