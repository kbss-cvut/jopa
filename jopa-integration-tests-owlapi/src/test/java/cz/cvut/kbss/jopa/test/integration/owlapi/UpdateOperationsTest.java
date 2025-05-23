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
package cz.cvut.kbss.jopa.test.integration.owlapi;

import cz.cvut.kbss.jopa.test.environment.OwlapiDataAccessor;
import cz.cvut.kbss.jopa.test.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.UpdateOperationsRunner;
import org.semanticweb.owlapi.model.OWLOntology;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;

public class UpdateOperationsTest extends UpdateOperationsRunner {

    private static final Logger LOG = LoggerFactory.getLogger(UpdateOperationsTest.class);

    private final OwlapiDataAccessor owlapiAccessor;

    public UpdateOperationsTest() {
        super(LOG, new OwlapiPersistenceFactory(), new OwlapiDataAccessor());
        this.owlapiAccessor = (OwlapiDataAccessor) dataAccessor;
    }

    @Override
    protected void verifyValueDatatype(URI identifier, String property, String expectedDatatype) {
        // OWL2Query does not support ASK with a FILTER, so we have to do the check using OWLAPI
        final OWLOntology ontology = em.unwrap(OWLOntology.class);
        owlapiAccessor.verifyValueDatatype(ontology, identifier, property, expectedDatatype);
    }
}
