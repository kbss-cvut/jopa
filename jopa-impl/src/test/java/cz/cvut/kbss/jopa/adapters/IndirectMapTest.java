/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.adapters;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.utils.Generators;

public class IndirectMapTest {

	private static OWLClassB owner;
	private static Field ownerField;
	private static Map<String, Set<String>> backupMap;

	@Mock
	private UnitOfWorkImpl uow;

	private Map<String, Set<String>> map;
	private IndirectMap<String, Set<String>> indirectMap;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		owner = new OWLClassB();
		owner.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityB"));
		ownerField = OWLClassB.getPropertiesField();
		backupMap = Generators.createProperties(15);
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		when(uow.isInTransaction()).thenReturn(Boolean.TRUE);
		this.map = new HashMap<>();
		map.putAll(backupMap);
		this.indirectMap = new IndirectMap<>(owner, ownerField, uow, map);

	}

	@Test(expected = NullPointerException.class)
	public void testConstructorNullUoW() {
		this.indirectMap = new IndirectMap<>(owner, ownerField, null, map);
	}

	@Test(expected = NullPointerException.class)
	public void testConstructorNullMap() {
		this.indirectMap = new IndirectMap<>(owner, ownerField, uow, null);
	}

	@Test
	public void testPut() {
		final String key = "http://krizik.felk.cvut.cz/ontologies/properties/p";
		final String value = "someDataPropertyValue";
		indirectMap.put(key, Collections.singleton(value));
		verify(uow).attributeChanged(owner, ownerField);
		assertTrue(map.containsKey(key));
	}

	@Test
	public void testRemove() {
		final String key = map.keySet().iterator().next();
		indirectMap.remove(key);
		verify(uow).attributeChanged(owner, ownerField);
		assertFalse(map.containsKey(key));
	}

	@Test
	public void testPutAll() {
		final Map<String, Set<String>> newMap = Generators.createProperties();
		indirectMap.putAll(newMap);
		verify(uow).attributeChanged(owner, ownerField);
		for (String key : newMap.keySet()) {
			assertTrue(map.containsKey(key));
		}
	}

	@Test
	public void testClear() {
		indirectMap.clear();
		verify(uow).attributeChanged(owner, ownerField);
		assertTrue(map.isEmpty());
	}
}
