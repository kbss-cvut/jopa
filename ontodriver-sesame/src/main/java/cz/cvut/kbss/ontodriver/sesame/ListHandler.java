package cz.cvut.kbss.ontodriver.sesame;

import java.util.Collection;

import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.descriptors.ListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;

/**
 * Base class for list handlers. </p>
 * 
 * List handlers are responsible for loading and persisting lists.
 * 
 * @author ledvima1
 * 
 * @param <T>
 *            List descriptor type
 * @param <V>
 *            List value descriptor type
 */
abstract class ListHandler<T extends ListDescriptor, V extends ListValueDescriptor> {

	protected final Connector connector;
	protected final ValueFactory vf;

	ListHandler(Connector connector, ValueFactory vf) {
		this.connector = connector;
		this.vf = vf;
	}

	/**
	 * Loads axioms representing list described by the specified list
	 * descriptor.
	 * 
	 * @return Collection of axioms representing sequence values
	 * @throws SesameDriverException
	 */
	abstract Collection<Axiom<?>> loadList(T listDescriptor) throws SesameDriverException;

	/**
	 * Persists list values specified by the descriptor. </p>
	 * 
	 * The values are saved in the order in which they appear in the descriptor.
	 * 
	 * @param listValueDescriptor
	 *            Describes values to persist
	 * @throws SesameDriverException
	 */
	abstract void persistList(V listValueDescriptor) throws SesameDriverException;

	/**
	 * Creates handler for simple lists.
	 * 
	 * @param listDescriptor
	 *            List descriptor
	 * @return List handler
	 */
	static ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> createForSimpleList(
			Connector connector, ValueFactory vf) {
		assert connector != null;
		assert vf != null;

		return new SimpleListHandler(connector, vf);
	}

	static ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> createForReferencedList(
			Connector connector, ValueFactory vf) {
		// TODO
		return null;
	}
}
