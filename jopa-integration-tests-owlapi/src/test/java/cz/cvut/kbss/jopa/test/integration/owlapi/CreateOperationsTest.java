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
package cz.cvut.kbss.jopa.test.integration.owlapi;

import cz.cvut.kbss.jopa.test.environment.OwlapiDataAccessor;
import cz.cvut.kbss.jopa.test.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.CreateOperationsRunner;
import org.semanticweb.owlapi.model.OWLOntology;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;

public class CreateOperationsTest extends CreateOperationsRunner {

    private static final Logger LOG = LoggerFactory.getLogger(CreateOperationsTest.class);

    private final OwlapiDataAccessor owlapiAccessor;

    public CreateOperationsTest() {
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
