/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.change.TransactionalChange;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLProperty;

import java.util.Collection;
import java.util.List;

abstract class OwlapiListIterator<T> {

    abstract boolean hasNext();

    abstract Axiom<T> next();

    abstract T nextValue();

    /**
     * Gets the current list node.
     * <p>
     * Gets the current list node, which for simple lists is the same as {@link #nextValue()}, but for referenced list,
     * it returns the actual node in the list, not its value.
     *
     * @return Current list node
     */
    abstract NamedResource getCurrentNode();

    /**
     * Removes the current element without reconnecting the previous node to the following one.
     * <p>
     * This effectively breaks the list. It should be used only for removing all the following nodes from the list.
     * <p>
     * Note that this method just creates the changes, it does not apply them.
     *
     * @return List of changes to apply
     */
    abstract List<TransactionalChange> removeWithoutReconnect();

    /**
     * Replaces the current value with the specified one.
     * <p>
     * Note that this method just creates the changes, it does not apply them.
     *
     * @param newValue The new value to use
     * @return List of changes to apply
     */
    abstract List<TransactionalChange> replaceNode(T newValue);

    static void checkMaxSuccessors(OWLProperty property, Collection<? extends OWLObject> successors) {
        if (successors.size() > 1) {
            throw new IntegrityConstraintViolatedException(
                    "Invalid number of successors. Expected only 1 value of property " + property + ", but got " +
                            successors.size());
        }
    }

    static void checkIsNamed(OWLIndividual individual) {
        if (!individual.isNamed()) {
            throw new IllegalArgumentException("Expected OWLNamedIndividual, but got an anonymous one.");
        }
    }
}
