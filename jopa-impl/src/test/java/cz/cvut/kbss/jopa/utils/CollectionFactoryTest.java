/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.util.List;
import java.util.Set;

import static org.junit.Assert.*;

public class CollectionFactoryTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void createDefaultCollectionReturnsListImplementationForListType() {
        assertTrue(CollectionFactory.createDefaultCollection(PluralAttribute.CollectionType.LIST) instanceof List);
    }

    @Test
    public void createDefaultCollectionReturnsSetImplementationForSetType() {
        assertTrue(CollectionFactory.createDefaultCollection(PluralAttribute.CollectionType.SET) instanceof Set);
    }

    @Test
    public void createDefaultCollectionReturnsSetImplementationForCollectionType() {
        assertTrue(CollectionFactory.createDefaultCollection(PluralAttribute.CollectionType.COLLECTION) instanceof Set);
    }

    @Test
    public void createDefaultCollectionThrowsIllegalArgumentForMapType() {
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage("Collection type " + PluralAttribute.CollectionType.MAP + " is not supported.");
        CollectionFactory.createDefaultCollection(PluralAttribute.CollectionType.MAP);
    }
}