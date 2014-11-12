package cz.cvut.kbss.ontodriver.sesame;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

class SimpleListHandler extends ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> {

	SimpleListHandler(Connector connector, ValueFactory vf) {
		super(connector, vf);
	}

	@Override
	Collection<Axiom<?>> loadList(SimpleListDescriptor listDescriptor) throws SesameDriverException {
		final Collection<Axiom<?>> axioms = new ArrayList<>();
		final SimpleListIterator it = new SimpleListIterator(listDescriptor, connector, vf);
		while (it.hasNext()) {
			axioms.add(it.nextAxiom());
		}
		return axioms;
	}

	@Override
	void persistList(SimpleListValueDescriptor listValueDescriptor) throws SesameDriverException {
		if (listValueDescriptor.getValues().isEmpty()) {
			return;
		}
		final Collection<Statement> stmts = new ArrayList<>();
		stmts.add(createListHead(listValueDescriptor));
		stmts.addAll(createListRest(listValueDescriptor));
		connector.addStatements(stmts);
	}

	private Statement createListHead(SimpleListValueDescriptor listValueDescriptor) {
		final Resource firstNode = sesameUri(listValueDescriptor.getValues().get(0).getIdentifier());
		return vf.createStatement(owner(listValueDescriptor), hasList(listValueDescriptor),
				firstNode, context(listValueDescriptor));
	}

	private List<Statement> createListRest(SimpleListValueDescriptor listValueDescriptor) {
		final List<Statement> stmts = new ArrayList<>(listValueDescriptor.getValues().size());
		NamedResource previous = listValueDescriptor.getValues().get(0);
		final URI nextNodeProp = hasNext(listValueDescriptor);
		final URI context = context(listValueDescriptor);
		for (NamedResource elem : listValueDescriptor.getValues()) {
			if (elem == previous) {
				continue;
			}
			final Resource subject = sesameUri(previous.getIdentifier());
			final Resource object = sesameUri(elem.getIdentifier());
			stmts.add(vf.createStatement(subject, nextNodeProp, object, context));
			previous = elem;
		}
		return stmts;
	}

	/**
	 * We are using this code instead of iterator.remove for performance
	 * reasons. The iterator has to reconnect the list for each removed node,
	 * which takes a lot of time.
	 */
	@Override
	void clearList(SimpleListValueDescriptor listValueDescriptor) throws SesameDriverException {
		final URI context = context(listValueDescriptor);
		final Collection<Statement> toRemove = new ArrayList<>();
		URI currentProperty = hasList(listValueDescriptor);
		final URI hasNext = hasNext(listValueDescriptor);
		final boolean includeInferred = listValueDescriptor.getNextNode().isInferred();
		Collection<Statement> stmts;
		Resource subject = owner(listValueDescriptor);
		do {
			stmts = connector.findStatements(subject, currentProperty, null, includeInferred,
					context);
			if (!stmts.isEmpty()) {
				subject = extractListNode(stmts, hasNext);
				toRemove.addAll(stmts);
			}
			currentProperty = hasNext;
		} while (!stmts.isEmpty());
		connector.removeStatements(toRemove);
	}

	@Override
	void mergeList(SimpleListValueDescriptor listDescriptor) throws SesameDriverException {
		final SimpleListIterator it = new SimpleListIterator(listDescriptor, connector, vf);
		int i = 0;
		while (it.hasNext() && i < listDescriptor.getValues().size()) {
			final Resource node = it.nextNode();
			final NamedResource newNode = listDescriptor.getValues().get(i);
			if (!node.stringValue().equals(newNode.getIdentifier().toString())) {
				it.replaceCurrentWith(newNode);
			}
			i++;
		}
		while (it.hasNext()) {
			it.nextNode();
			it.remove();
		}
		assert i > 0;
		final Collection<Statement> toAdd = new ArrayList<>(listDescriptor.getValues().size() - i);
		final URI context = context(listDescriptor);
		while (i < listDescriptor.getValues().size()) {
			final NamedResource previous = listDescriptor.getValues().get(i - 1);
			final NamedResource newNode = listDescriptor.getValues().get(i);
			final Statement stmt = vf.createStatement(sesameUri(previous.getIdentifier()),
					sesameUri(listDescriptor.getNextNode().getIdentifier()),
					sesameUri(newNode.getIdentifier()), context);
			toAdd.add(stmt);
			i++;
		}
		connector.addStatements(toAdd);
	}
}
