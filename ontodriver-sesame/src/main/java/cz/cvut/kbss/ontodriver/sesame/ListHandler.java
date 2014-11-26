package cz.cvut.kbss.ontodriver.sesame;

import java.util.Collection;

import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.ontodriver.exceptions.IntegrityConstraintViolatedException;
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
	abstract Collection<Axiom<NamedResource>> loadList(T listDescriptor) throws SesameDriverException;

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
	 * Updates list with values specified by the descriptor. </p>
	 * 
	 * @param listValueDescriptor
	 *            Describes the updated values
	 * @throws SesameDriverException
	 */
	void updateList(V listValueDescriptor) throws SesameDriverException {
		if (listValueDescriptor.getValues().isEmpty()) {
			clearList(listValueDescriptor);
		} else if (isOldListEmpty(owner(listValueDescriptor), hasList(listValueDescriptor),
				listValueDescriptor.getListProperty().isInferred(), context(listValueDescriptor))) {
			persistList(listValueDescriptor);
		} else {
			mergeList(listValueDescriptor);
		}
	}

	private boolean isOldListEmpty(Resource owner, URI hasListProperty, boolean includeInferred,
			URI context) throws SesameDriverException {
		final Collection<Statement> stmts = connector.findStatements(owner, hasListProperty, null,
				includeInferred, context);
		return stmts.isEmpty();
	}

	abstract void clearList(V listDescriptor) throws SesameDriverException;

	abstract void mergeList(V listDescriptor) throws SesameDriverException;

	protected Resource extractListNode(Collection<Statement> stmts, URI nodeAssertion)
			throws SesameDriverException {
		if (stmts.size() > 1) {
			throw new IntegrityConstraintViolatedException(
					"Invalid number of values found for assertion " + nodeAssertion
							+ ". Expected 1, got " + stmts.size());
		}
		final Value val = stmts.iterator().next().getObject();
		if (!(val instanceof Resource)) {
			throw new IntegrityConstraintViolatedException(
					"Invalid property value. Expected object property value, got literal.");
		}
		return (Resource) val;
	}

	protected URI context(ListDescriptor listDescriptor) {
		return sesameUri(listDescriptor.getContext());
	}

	protected URI owner(ListDescriptor listDescriptor) {
		return sesameUri(listDescriptor.getListOwner().getIdentifier());
	}

	protected URI hasList(ListDescriptor listDescriptor) {
		return sesameUri(listDescriptor.getListProperty().getIdentifier());
	}

	protected URI hasNext(ListDescriptor listDescriptor) {
		return sesameUri(listDescriptor.getNextNode().getIdentifier());
	}

	protected URI sesameUri(java.net.URI uri) {
		return SesameUtils.toSesameUri(uri, vf);
	}

	/**
	 * Creates handler for simple lists.
	 * 
	 * @param connector
	 *            Storage connector
	 * @param vf
	 *            Sesame value factory
	 * @return List handler
	 */
	static ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> createForSimpleList(
			Connector connector, ValueFactory vf) {
		assert connector != null;
		assert vf != null;

		return new SimpleListHandler(connector, vf);
	}

	/**
	 * Creates handler for referenced lists.
	 * 
	 * @param connector
	 *            Storage connector
	 * @param vf
	 *            Sesame value factory
	 * @return List handler
	 */
	static ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> createForReferencedList(
			Connector connector, ValueFactory vf) {
		assert connector != null;
		assert vf != null;

		return new ReferencedListHandler(connector, vf);
	}
}
