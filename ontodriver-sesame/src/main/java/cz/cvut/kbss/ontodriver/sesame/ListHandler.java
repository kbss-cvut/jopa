package cz.cvut.kbss.ontodriver.sesame;

import java.util.Collection;

import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.SimpleListDescriptor;
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
 */
abstract class ListHandler<T extends SimpleListDescriptor> {

	protected final T listDescriptor;
	protected final Connector connector;
	protected final ValueFactory vf;

	ListHandler(T listDescriptor, Connector connector, ValueFactory vf) {
		this.listDescriptor = listDescriptor;
		this.connector = connector;
		this.vf = vf;
	}

	abstract Collection<Axiom<?>> loadList() throws SesameDriverException;

	/**
	 * Creates handler for simple lists.
	 * 
	 * @param listDescriptor
	 *            List descriptor
	 * @return List handler
	 */
	static ListHandler<SimpleListDescriptor> createForSimpleList(
			SimpleListDescriptor listDescriptor, Connector connector, ValueFactory vf) {
		assert listDescriptor != null;
		assert connector != null;
		assert vf != null;

		return new SimpleListHandler(listDescriptor, connector, vf);
	}

	static ListHandler<ReferencedListDescriptor> createForReferencedList(
			ReferencedListDescriptor listDescriptor, Connector connector, ValueFactory vf) {
		// TODO
		return null;
	}
}
