package cz.cvut.kbss.owlpersistence.model.metamodel;

import cz.cvut.kbss.owlpersistence.NonJPA;
import cz.cvut.kbss.owlpersistence.model.IRI;
import cz.cvut.kbss.owlpersistence.model.annotations.SequenceType;

/**
 * Instances of the type ListAttribute represent persistent
 * java.util.List-valued attributes.
 * 
 * @param <X>
 *            The type the represented List belongs to
 * @param <E>
 *            The element type of the represented List
 */
public interface ListAttribute<X, E> extends
		PluralAttribute<X, java.util.List<E>, E> {

	@NonJPA
	public SequenceType getSequenceType();

	@NonJPA
	public IRI getOWLListClass();

	@NonJPA
	public IRI getOWLPropertyHasContentsIRI();

	@NonJPA
	public IRI getOWLObjectPropertyHasNextIRI();
}
