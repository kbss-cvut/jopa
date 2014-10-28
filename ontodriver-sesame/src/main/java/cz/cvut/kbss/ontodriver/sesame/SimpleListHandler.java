package cz.cvut.kbss.ontodriver.sesame;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.ontodriver.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.AxiomImpl;

class SimpleListHandler extends ListHandler<SimpleListDescriptor> {

	SimpleListHandler(SimpleListDescriptor listDescriptor, Connector connector, ValueFactory vf) {
		super(listDescriptor, connector, vf);
	}

	@Override
	Collection<Axiom<?>> loadList() throws SesameDriverException {
		final Collection<Axiom<?>> axioms = new ArrayList<>();
		final Axiom<java.net.URI> head = loadListHead();
		if (head == null) {
			return Collections.emptyList();
		}
		axioms.add(head);
		final Resource headElem = SesameUtils.toSesameUri(head.getValue().getValue(), vf);
		axioms.addAll(loadListRest(headElem));
		return axioms;
	}

	private Axiom<java.net.URI> loadListHead() throws SesameDriverException {
		final URI context = SesameUtils.toSesameUri(listDescriptor.getContext(), vf);
		final URI hasListProperty = SesameUtils.toSesameUri(listDescriptor.getListProperty()
				.getIdentifier(), vf);
		final URI owner = SesameUtils
				.toSesameUri(listDescriptor.getListOwner().getIdentifier(), vf);
		Collection<Statement> stmts = connector.findStatements(owner, hasListProperty, null,
				listDescriptor.getListProperty().isInferred(), context);
		if (stmts.isEmpty()) {
			return null;
		}
		final Resource head = getListNode(stmts);
		final java.net.URI javaHead = SesameUtils.toJavaUri(head);
		return createAxiom(javaHead);
	}

	private Resource getListNode(Collection<Statement> stmts) throws SesameDriverException {
		if (stmts.size() > 1) {
			throw new IntegrityConstraintViolatedException(
					"Invalid number of values found for assertion "
							+ listDescriptor.getListProperty() + ". Expected 1, got "
							+ stmts.size());
		}
		final Value val = stmts.iterator().next().getObject();
		if (!(val instanceof Resource)) {
			throw new IntegrityConstraintViolatedException(
					"Invalid property value. Expected object property value, got literal.");
		}
		return (Resource) val;
	}

	private Axiom<java.net.URI> createAxiom(final java.net.URI nodeValue) {
		return new AxiomImpl<java.net.URI>(listDescriptor.getListOwner(),
				listDescriptor.getListProperty(),
				new cz.cvut.kbss.ontodriver_new.model.Value<java.net.URI>(nodeValue));
	}

	private Collection<Axiom<?>> loadListRest(Resource firstElem) throws SesameDriverException {
		final Collection<Axiom<?>> axioms = new ArrayList<>();
		final URI context = SesameUtils.toSesameUri(listDescriptor.getContext(), vf);
		Resource subject = firstElem;
		final URI nextElemProperty = SesameUtils.toSesameUri(listDescriptor.getNextNode()
				.getIdentifier(), vf);
		Collection<Statement> stmts = null;
		do {
			stmts = connector.findStatements(subject, nextElemProperty, null, listDescriptor
					.getNextNode().isInferred(), context);
			if (!stmts.isEmpty()) {
				final Resource listNode = getListNode(stmts);
				axioms.add(createAxiom(SesameUtils.toJavaUri(listNode)));
				subject = listNode;
			}
		} while (!stmts.isEmpty());
		return axioms;
	}
}
