/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions.merge;

import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecordImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.HashMap;
import java.util.HashSet;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

@ExtendWith({MockitoExtension.class})
@MockitoSettings(strictness = Strictness.LENIENT)
public class MapValueMergerTest {

    @Mock
    private Metamodel metamodel;

    private FieldSpecification<? super OWLClassB, ?> propertiesSpec;

    private Descriptor descriptor;

    private MapValueMerger merger;

    @BeforeEach
    public void setUp() throws Exception {
        final MetamodelMocks mocks = new MetamodelMocks();
        mocks.setMocks(metamodel);
        this.descriptor = new EntityDescriptor();
        final EntityType<OWLClassB> et = metamodel.entity(OWLClassB.class);
        this.propertiesSpec = et.getProperties();

        this.merger = new MapValueMerger();
    }

    @Test
    public void mergeMergesPropertiesMap() {
        final OWLClassB orig = new OWLClassB(Generators.createIndividualIdentifier());
        orig.setProperties(Generators.generateStringProperties());
        final OWLClassB merged = new OWLClassB(orig.getUri());
        merged.setProperties(new HashMap<>(orig.getProperties()));
        final String newKey = Generators.createPropertyIdentifier().toString();
        merged.getProperties().put(newKey, new HashSet<>());
        final String vOne = "valueOne";
        final String vTwo = "valueTwo";
        merged.getProperties().get(newKey).add(vOne);
        merged.getProperties().get(newKey).add(vTwo);

        merger.mergeValue(orig, new ChangeRecordImpl(propertiesSpec, merged.getProperties()), descriptor);
        assertEquals(merged.getProperties(), orig.getProperties());
    }

    @Test
    public void mergeSetsFieldValueToNullWhenMergedInstanceValueIsNull() {
        final OWLClassB orig = new OWLClassB(Generators.createIndividualIdentifier());
        orig.setProperties(Generators.generateStringProperties());
        final OWLClassB merged = new OWLClassB(orig.getUri());
        merged.setProperties(null);

        merger.mergeValue(orig, new ChangeRecordImpl(propertiesSpec, merged.getProperties()), descriptor);
        assertNull(orig.getProperties());
    }
}
