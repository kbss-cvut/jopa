/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions.merge;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecordImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertEquals;

@MockitoSettings(strictness = Strictness.LENIENT)
class DefaultValueMergerTest {

    private MetamodelMocks metamodelMocks;

    private Descriptor descriptor;

    private DefaultValueMerger sut;

    @BeforeEach
    void setUp() throws Exception {
        this.metamodelMocks = new MetamodelMocks();
        this.descriptor = new EntityDescriptor();

        this.sut = new DefaultValueMerger();
    }

    @Test
    void mergeValueSetsValueFromChangeRecordDirectlyOnTargetObject() {
        final OWLClassA target = Generators.generateOwlClassAInstance();
        final String newValue = "new test value";
        sut.mergeValue(target, new ChangeRecordImpl(metamodelMocks.forOwlClassA().stringAttribute(), newValue), descriptor);
        assertEquals(newValue, target.getStringAttribute());
    }
}
