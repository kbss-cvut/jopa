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
package cz.cvut.kbss.ontodriver.sesame;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.lang.reflect.Field;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.ontodriver.Connection;

public class SesameDataSourceTest {

	@Mock
	private SesameDriver driverMock;

	private SesameDataSource dataSource;

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		this.dataSource = new SesameDataSource();
		TestUtils.setMock("driver", dataSource, driverMock);
	}

	@Test(expected = NullPointerException.class)
	public void testSesameDataSourceSetNullStorageProperties() throws OntoDriverException {
		final SesameDataSource ds = new SesameDataSource();
		ds.setStorageProperties(null);
		fail("This line should not have been reached.");
		assert ds == null;
	}

	@Test
	public void testClose() throws Exception {
		assertTrue(dataSource.isOpen());
		dataSource.close();
		assertFalse(dataSource.isOpen());
		dataSource.close();
		assertFalse(dataSource.isOpen());
	}

	@Test
	public void testGetConnection() throws Exception {
		Field connected = dataSource.getClass().getDeclaredField("connected");
		connected.setAccessible(true);
		connected.set(dataSource, true);
		final Connection res = dataSource.getConnection();
		verify(driverMock).acquireConnection();
		assertNull(res);
	}

	@Test(expected = IllegalStateException.class)
	public void testGetConnectionWithoutInitialization() throws Exception {
		final Connection res = dataSource.getConnection();
		// This shouldn't be reached
		assert res == null;
	}

	@Test(expected = IllegalStateException.class)
	public void testGetConnectionOnClosed() throws Exception {
		dataSource.close();
		assertFalse(dataSource.isOpen());
		try {
			dataSource.getConnection();
		} finally {
			verify(driverMock, never()).acquireConnection();
		}
	}
}
