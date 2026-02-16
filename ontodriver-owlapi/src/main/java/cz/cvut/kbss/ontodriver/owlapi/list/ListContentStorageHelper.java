/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Translations;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.owlapi.AxiomAdapter;
import cz.cvut.kbss.ontodriver.owlapi.change.MutableAddAxiom;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.List;

/**
 * Helper for storing list/container content.
 */
public class ListContentStorageHelper {

    private final AxiomAdapter axiomAdapter;
    private final OWLOntology ontology;

    public ListContentStorageHelper(AxiomAdapter axiomAdapter, OWLOntology ontology) {
        this.axiomAdapter = axiomAdapter;
        this.ontology = ontology;
    }

    /**
     * Generates OWL axioms corresponding to the specified list/container value.
     *
     * @param subject   List owner
     * @param assertion Content property
     * @param value     Value to store
     * @return List of axioms to add to the ontology
     */
    public List<MutableAddAxiom> saveListValue(NamedResource subject, Assertion assertion, Object value) {
        return switch (assertion.getType()) {
            case OBJECT_PROPERTY ->
                    List.of(new MutableAddAxiom(ontology, axiomAdapter.toOwlObjectPropertyAssertionAxiom(new AxiomImpl<>(subject, assertion, new Value<>(value)))));
            case DATA_PROPERTY -> toDataPropertyValues(subject, assertion, value);
            default -> throw new IllegalArgumentException("Unsupported property type " + assertion.getType());
        };
    }

    private List<MutableAddAxiom> toDataPropertyValues(NamedResource subject, Assertion assertion, Object value) {
        if (value instanceof Translations mls) {
            return mls.getValue().entrySet().stream()
                      .map(e -> axiomAdapter.toOwlDataPropertyAssertionAxiom(new AxiomImpl<>(subject, assertion, new Value<>(new LangString(e.getValue(), e.getKey())))))
                      .map(ax -> new MutableAddAxiom(ontology, ax)).toList();
        } else {
            return List.of(new MutableAddAxiom(ontology, axiomAdapter.toOwlDataPropertyAssertionAxiom(new AxiomImpl<>(subject, assertion, new Value<>(value)))));
        }
    }
}
