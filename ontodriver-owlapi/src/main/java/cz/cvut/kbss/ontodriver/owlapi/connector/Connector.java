/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi.connector;

import cz.cvut.kbss.ontodriver.owlapi.change.TransactionalChange;

import java.net.URI;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * Storage connector interface.
 * <p>
 * This interface declares the methods accessible from the driver.
 */
public interface Connector {

    /**
     * Gets logical URI of the ontology loaded by this connector.
     *
     * @return Ontology URI
     */
    URI getOntologyUri();

    /**
     * Gets snapshot of the underlying ontology.
     * <p>
     * The snapshot is completely independent of the live ontology, so any changes to either are not visible to the
     * other.
     *
     * @return Value object with the ontology snapshot
     */
    OntologySnapshot getOntologySnapshot();

    /**
     * Executes read-only operation on the live ontology.
     *
     * @param function The function to execute
     * @param <R>      Result type
     * @return Read result
     */
    <R> R executeRead(Function<OntologySnapshot, R> function);

    /**
     * Executes a write operation on the live ontology.
     *
     * @param function The function to execute
     */
    void executeWrite(Consumer<OntologySnapshot> function);

    /**
     * Applies the specified changes to the underlying ontology.
     * <p>
     * Note that this operation is atomic - the changes are applied as a whole and no other operation can be performed
     * on the underlying ontology while the changes are being applied.
     *
     * @param changes The changes to apply
     */
    void applyChanges(List<TransactionalChange> changes);

    /**
     * Closes the specified transactional snapshot.
     * <p>
     * This in particular means destroying the transactional ontology.
     *
     * @param snapshot The snapshot to close
     */
    void closeSnapshot(OntologySnapshot snapshot);
}
