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
import cz.cvut.kbss.jopa.test.runner.DeleteOperationsWithInheritanceRunner;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;

public class DeleteOperationsWithInheritanceTest extends DeleteOperationsWithInheritanceRunner {

    private static final Logger LOG = LoggerFactory.getLogger(DeleteOperationsWithInheritanceTest.class);

    public DeleteOperationsWithInheritanceTest() {
        super(LOG, new OwlapiPersistenceFactory(), new OwlapiDataAccessor());
    }

    /**
     * Ignored due to bug in OWL2Query, see {@link #verifyIndividualWasRemoved(URI)}
     */
    @Disabled
    @Test
    @Override
    public void testRemoveEntityWithMappedSuperclass() {
        super.testRemoveEntityWithMappedSuperclass();
    }

    /**
     * Ignored due to bug in OWL2Query, see {@link #verifyIndividualWasRemoved(URI)}
     */
    @Disabled
    @Test
    @Override
    public void testRemoveEntityWithEntitySuperclass() {
        super.testRemoveEntityWithEntitySuperclass();
    }
}
