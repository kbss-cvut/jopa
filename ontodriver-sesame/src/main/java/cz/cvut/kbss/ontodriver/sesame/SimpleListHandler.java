package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

class SimpleListHandler extends ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> {

    SimpleListHandler(Connector connector, ValueFactory vf) {
        super(connector, vf);
    }

    @Override
    SesameIterator createIterator(SimpleListDescriptor listDescriptor) throws SesameDriverException {
        return new SimpleListIterator(listDescriptor, connector, vf);
    }

    URI createListHead(SimpleListValueDescriptor listValueDescriptor, Collection<Statement> listStatements) {
        final URI firstNode = sesameUri(listValueDescriptor.getValues().get(0).getIdentifier());
        listStatements.add(vf.createStatement(owner(listValueDescriptor), hasList(listValueDescriptor),
                firstNode, context(listValueDescriptor)));
        return firstNode;
    }

    Collection<Statement> createListRest(URI head, SimpleListValueDescriptor listValueDescriptor) {
        final Collection<Statement> statements = new ArrayList<>(listValueDescriptor.getValues().size());
        URI previous = head;
        final URI nextNodeProp = hasNext(listValueDescriptor);
        final URI context = context(listValueDescriptor);
        final Iterator<NamedResource> it = listValueDescriptor.getValues().iterator();
        it.next();
        while (it.hasNext()) {
            final URI object = sesameUri(it.next().getIdentifier());
            statements.add(vf.createStatement(previous, nextNodeProp, object, context));
            previous = object;
        }
        return statements;
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
    MergeResult mergeWithOriginalList(SimpleListValueDescriptor listDescriptor, SesameIterator it) throws SesameDriverException {
        int i = 0;
        Resource node = null;
        while (it.hasNext() && i < listDescriptor.getValues().size()) {
            node = it.nextNode();
            final NamedResource newNode = listDescriptor.getValues().get(i);
            if (!node.stringValue().equals(newNode.getIdentifier().toString())) {
                node = sesameUri(newNode.getIdentifier());
                it.replaceCurrentWith(newNode);
            }
            i++;
        }
        return new MergeResult(i, node);
    }

    @Override
    void appendNewNodes(SimpleListValueDescriptor listDescriptor, MergeResult mergeResult) throws SesameDriverException {
        int i = mergeResult.i;
        final Collection<Statement> toAdd = new ArrayList<>(listDescriptor.getValues().size() - i);
        Resource previous = mergeResult.previous;
        final URI nextNode = sesameUri(listDescriptor.getNextNode().getIdentifier());
        final URI context = context(listDescriptor);
        while (i < listDescriptor.getValues().size()) {
            final Resource newNode = sesameUri(listDescriptor.getValues().get(i).getIdentifier());
            final Statement stmt = vf.createStatement(previous,
                    nextNode, newNode, context);
            toAdd.add(stmt);
            previous = newNode;
            i++;
        }
        connector.addStatements(toAdd);
    }

    @Override
    SesameIterator iterator(SimpleListValueDescriptor listDescriptor) throws SesameDriverException {
        return new SimpleListIterator(listDescriptor, connector, vf);
    }
}
