/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.descriptor.*;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.connector.Connector;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.util.Rdf4jUtils;
import org.eclipse.rdf4j.model.*;

import java.util.*;

/**
 * Base class for list handlers.
 * <p>
 * List handlers are responsible for loading and persisting lists.
 *
 * @param <T> List descriptor type
 * @param <V> List value descriptor type
 */
abstract class ListHandler<T extends ListDescriptor, V extends ListValueDescriptor> {

    protected final Connector connector;
    protected final ValueFactory vf;

    ListHandler(Connector connector, ValueFactory vf) {
        this.connector = connector;
        this.vf = vf;
    }

    /**
     * Loads axioms representing list described by the specified list descriptor.
     *
     * @return Collection of axioms representing sequence values
     * @throws Rdf4jDriverException When storage access error occurs
     */
    List<Axiom<NamedResource>> loadList(T listDescriptor) throws Rdf4jDriverException {
        final List<Axiom<NamedResource>> axioms = new ArrayList<>();
        final ListIterator it = createIterator(listDescriptor);
        while (it.hasNext()) {
            axioms.add(it.nextAxiom());
        }
        return axioms;
    }

    abstract ListIterator createIterator(T listDescriptor) throws Rdf4jDriverException;

    /**
     * Persists list values specified by the descriptor.
     * <p>
     * The values are saved in the order in which they appear in the descriptor.
     *
     * @param listValueDescriptor Describes values to persist
     * @throws Rdf4jDriverException When storage access error occurs
     */
    void persistList(V listValueDescriptor) throws Rdf4jDriverException {
        if (listValueDescriptor.getValues().isEmpty()) {
            return;
        }
        final Collection<Statement> statements = new ArrayList<>(listValueDescriptor.getValues()
                                                                                    .size());
        final IRI head = createListHead(listValueDescriptor, statements);
        statements.addAll(createListRest(head, listValueDescriptor));
        connector.addStatements(statements);
    }

    abstract IRI createListHead(V valueDescriptor, Collection<Statement> listStatements) throws Rdf4jDriverException;

    abstract List<Statement> createListRest(IRI head, V valueDescriptor) throws Rdf4jDriverException;

    /**
     * Updates list with values specified by the descriptor.
     *
     * @param listValueDescriptor Describes the updated values
     * @throws Rdf4jDriverException When storage access error occurs
     */
    void updateList(V listValueDescriptor) throws Rdf4jDriverException {
        if (listValueDescriptor.getValues().isEmpty()) {
            clearList(listValueDescriptor);
        } else if (isOldListEmpty(owner(listValueDescriptor), hasList(listValueDescriptor),
                listValueDescriptor.getListProperty().isInferred(), contexts(listValueDescriptor))) {
            persistList(listValueDescriptor);
        } else {
            mergeList(listValueDescriptor);
        }
    }

    private boolean isOldListEmpty(Resource owner, IRI hasListProperty, boolean includeInferred,
                                   Set<IRI> contexts) throws Rdf4jDriverException {
        final Collection<Statement> stmts = connector.findStatements(owner, hasListProperty, null,
                includeInferred, contexts);
        return stmts.isEmpty();
    }

    abstract void clearList(V listDescriptor) throws Rdf4jDriverException;

    private void mergeList(V listDescriptor) throws Rdf4jDriverException {
        final ListIterator it = iterator(listDescriptor);
        final MergeResult mergeResult = mergeWithOriginalList(listDescriptor, it);
        removeObsoletes(it);
        assert mergeResult.i > 0;
        assert mergeResult.previous != null;
        if (mergeResult.i < listDescriptor.getValues().size()) {
            appendNewNodes(listDescriptor, mergeResult);
        }
    }

    abstract ListIterator iterator(V listDescriptor) throws Rdf4jDriverException;

    abstract MergeResult mergeWithOriginalList(V listDescriptor, ListIterator it) throws Rdf4jDriverException;

    abstract void appendNewNodes(V listDescriptor, MergeResult mergeResult) throws Rdf4jDriverException;

    private void removeObsoletes(ListIterator it) throws Rdf4jDriverException {
        while (it.hasNext()) {
            it.nextNode();
            it.remove();
        }
    }

    Resource extractListNode(Collection<Statement> stmts, IRI nodeAssertion) {
        if (stmts.size() > 1) {
            throw new IntegrityConstraintViolatedException(
                    "Invalid number of values found for assertion " + nodeAssertion + ". Expected 1, got " +
                            stmts.size());
        }
        final Value val = stmts.iterator().next().getObject();
        if (!(val instanceof Resource)) {
            throw new IntegrityConstraintViolatedException(
                    "Invalid property value. Expected object property value, got literal.");
        }
        return (Resource) val;
    }

    Set<IRI> contexts(ListDescriptor listDescriptor) {
        final IRI ctx = toRdf4jIri(listDescriptor.getContext());
        return ctx != null ? Collections.singleton(ctx) : Collections.emptySet();
    }

    IRI context(ListDescriptor listDescriptor) {
        return toRdf4jIri(listDescriptor.getContext());
    }

    IRI owner(ListDescriptor listDescriptor) {
        return toRdf4jIri(listDescriptor.getListOwner().getIdentifier());
    }

    IRI hasList(ListDescriptor listDescriptor) {
        return toRdf4jIri(listDescriptor.getListProperty().getIdentifier());
    }

    IRI hasNext(ListDescriptor listDescriptor) {
        return toRdf4jIri(listDescriptor.getNextNode().getIdentifier());
    }

    IRI toRdf4jIri(java.net.URI uri) {
        return Rdf4jUtils.toRdf4jIri(uri, vf);
    }

    /**
     * Creates handler for simple lists.
     *
     * @param connector Storage connector
     * @param vf        RDF4J value factory
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
     * @param connector Storage connector
     * @param vf        RDF4J value factory
     * @return List handler
     */
    static ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> createForReferencedList(
            Connector connector, ValueFactory vf) {
        assert connector != null;
        assert vf != null;

        return new ReferencedListHandler(connector, vf);
    }

    static final class MergeResult {
        final int i;
        final Resource previous;

        MergeResult(int i, Resource node) {
            this.i = i;
            this.previous = node;
        }
    }
}
