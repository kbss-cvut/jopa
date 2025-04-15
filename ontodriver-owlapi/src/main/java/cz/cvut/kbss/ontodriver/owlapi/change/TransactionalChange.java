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
package cz.cvut.kbss.ontodriver.owlapi.change;

import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;

import java.util.List;

/**
 * Represents a change in data made during a transaction.
 */
public interface TransactionalChange {

    /**
     * Translates this transactional change to corresponding OWLAPI change axioms.
     *
     * @param targetOntology Ontology to which the changes are applied
     * @return A list of OWL change axioms corresponding to this change
     */
    List<OWLOntologyChange> toOwlChanges(OWLOntology targetOntology);

    /**
     * Whether this change overrides the specified existing change.
     * <p>
     * The most typical case is a subject-property remove change overriding an existing add axiom asserting the
     * specified property value.
     * <p>
     * Note that a remove change need not be overriden by an add change, because if applied in a sequence, the addition
     * occurs after removal.
     *
     * @param existing Possibly overriden existing transactional change
     * @return {@code true} if this change overrides the specified one, {@code false} otherwise
     */
    default boolean overrides(TransactionalChange existing) {return false;}
}
