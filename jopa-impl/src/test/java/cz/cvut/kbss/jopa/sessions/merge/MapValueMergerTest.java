/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions.merge;

import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.HashMap;
import java.util.HashSet;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

@MockitoSettings(strictness = Strictness.LENIENT)
public class MapValueMergerTest {

    private MetamodelMocks metamodelMocks;

    private final Descriptor descriptor = new EntityDescriptor();

    private final MapValueMerger sut = new MapValueMerger();

    @BeforeEach
    public void setUp() throws Exception {
        this.metamodelMocks = new MetamodelMocks();
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

        sut.mergeValue(orig, new ChangeRecord(metamodelMocks.forOwlClassB().propertiesSpec(), merged.getProperties()), descriptor);
        assertEquals(merged.getProperties(), orig.getProperties());
    }

    @Test
    public void mergeSetsFieldValueToNullWhenMergedInstanceValueIsNull() {
        final OWLClassB orig = new OWLClassB(Generators.createIndividualIdentifier());
        orig.setProperties(Generators.generateStringProperties());
        final OWLClassB merged = new OWLClassB(orig.getUri());
        merged.setProperties(null);

        sut.mergeValue(orig, new ChangeRecord(metamodelMocks.forOwlClassB().propertiesSpec(), merged.getProperties()), descriptor);
        assertNull(orig.getProperties());
    }
}
