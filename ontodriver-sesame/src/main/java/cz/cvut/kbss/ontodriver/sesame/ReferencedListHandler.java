package cz.cvut.kbss.ontodriver.sesame;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.ontodriver.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.AxiomImpl;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;

public class ReferencedListHandler extends
		ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> {

	private ReferencedListDescriptor listDescriptor;

	ReferencedListHandler(Connector connector, ValueFactory vf) {
		super(connector, vf);
	}

	@Override
	Collection<Axiom<?>> loadList(ReferencedListDescriptor listDescriptor)
			throws SesameDriverException {
		this.listDescriptor = listDescriptor;
		final List<Axiom<?>> axioms = new ArrayList<>();
		Resource headNode = loadListHead(axioms);
		if (headNode == null) {
			return Collections.emptyList();
		}
		axioms.addAll(loadListRest(headNode));
		return axioms;
	}

	private Resource loadListHead(List<Axiom<?>> axioms) throws SesameDriverException {
		final URI owner = SesameUtils
				.toSesameUri(listDescriptor.getListOwner().getIdentifier(), vf);
		final URI hasList = SesameUtils.toSesameUri(listDescriptor.getListProperty()
				.getIdentifier(), vf);
		final URI context = SesameUtils.toSesameUri(listDescriptor.getContext(), vf);
		final Collection<Statement> heads = connector.findStatements(owner, hasList, null,
				listDescriptor.getListProperty().isInferred(), context);
		if (heads.isEmpty()) {
			return null;
		}
		final Resource headNode = extractListNode(heads, listDescriptor.getListProperty());
		final URI hasContent = SesameUtils.toSesameUri(listDescriptor.getNodeContent()
				.getIdentifier(), vf);
		final Resource head = getNodeContent(headNode, hasContent, context);
		axioms.add(createAxiom(owner, listDescriptor.getNodeContent(), head));
		return headNode;
	}

	private Resource getNodeContent(final Resource node, final URI hasContent, final URI context)
			throws SesameDriverException {
		final Collection<Statement> nodeContents = connector.findStatements(node, hasContent, null,
				listDescriptor.getNodeContent().isInferred(), context);
		if (nodeContents.isEmpty()) {
			throw new IntegrityConstraintViolatedException("Referenced list node " + node
					+ " is missing content.");
		}
		final Resource content = extractListNode(nodeContents, listDescriptor.getNodeContent());
		return content;
	}

	private Axiom<java.net.URI> createAxiom(Resource subject, Assertion assertion, Resource value) {
		return new AxiomImpl<java.net.URI>(NamedResource.create(SesameUtils.toJavaUri(subject)),
				assertion, new Value<java.net.URI>(SesameUtils.toJavaUri(value)));
	}

	private Collection<Axiom<java.net.URI>> loadListRest(Resource previousNode)
			throws SesameDriverException {
		final List<Axiom<java.net.URI>> axioms = new ArrayList<>();
		final URI context = SesameUtils.toSesameUri(listDescriptor.getContext(), vf);
		boolean includeInferred = listDescriptor.getNextNode().isInferred();
		final URI nextNode = SesameUtils.toSesameUri(listDescriptor.getNextNode().getIdentifier(),
				vf);
		final URI hasContent = SesameUtils.toSesameUri(listDescriptor.getNodeContent()
				.getIdentifier(), vf);
		boolean hasNext = true;
		do {
			final Collection<Statement> nextNodes = connector.findStatements(previousNode,
					nextNode, null, includeInferred, context);
			if (!nextNodes.isEmpty()) {
				Resource node = extractListNode(nextNodes, listDescriptor.getNextNode());
				final Resource content = getNodeContent(node, hasContent, context);
				axioms.add(createAxiom(node, listDescriptor.getNodeContent(), content));
				previousNode = node;
			} else {
				hasNext = false;
			}
		} while (hasNext);
		return axioms;
	}

	@Override
	void persistList(ReferencedListValueDescriptor listValueDescriptor)
			throws SesameDriverException {
		// TODO Auto-generated method stub

	}

}
