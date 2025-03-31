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
import cz.cvut.kbss.jopa.test.runner.RetrieveWithInferenceRunner;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RetrieveWithInferenceTest extends RetrieveWithInferenceRunner {

    private static final Logger LOG = LoggerFactory.getLogger(RetrieveWithInferenceTest.class);

    public RetrieveWithInferenceTest() {
        super(LOG, new OwlapiPersistenceFactory(), new OwlapiDataAccessor());
    }

    @Test
    @Override
    public void retrievedEntityWithInferredTypesContainsInferredData() throws Exception {
        this.em = getEntityManager("retrievedEntityWithInferredTypesContainsInferredData", false);
        super.retrievedEntityWithInferredTypesContainsInferredData();
    }

    @Test
    @Override
    public void isInferredReturnsTrueForInferredPropertyValue() throws Exception {
        this.em = getEntityManager("isInferredReturnsTrueForInferredPropertyValue", false);
        super.isInferredReturnsTrueForInferredPropertyValue();
    }

    @Test
    @Override
    public void findReturnsOnlyAssertedDataWhenDescriptorDisablesInference() throws Exception {
        this.em = getEntityManager("findReturnsOnlyAssertedDataWhenDescriptorDisablesInference", false);
        super.findReturnsOnlyAssertedDataWhenDescriptorDisablesInference();
    }

    @Test
    @Override
    public void selectQueryWithDisabledInferenceAppliesThisSettingToLoadedResultsAsWell() throws Exception {
        this.em = getEntityManager("selectQueryWithDisabledInferenceAppliesThisSettingToLoadedResultsAsWell", false);
        super.selectQueryWithDisabledInferenceAppliesThisSettingToLoadedResultsAsWell();
    }
}
