/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j.connector;

import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.sail.SailRepository;
import org.eclipse.rdf4j.sail.memory.MemoryStore;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;

public class ConnectorFactoryImplTest {

    @Test
    public void setRepositoryThrowsIllegalStateWhenCalledOnClosedFactory() throws Exception {
        final StorageConnector connector = mock(StorageConnector.class);
        final ConnectorFactory sut = new ConnectorFactoryImpl(connector);
        sut.close();
        final Repository repo = new SailRepository(new MemoryStore());
        try {
            assertThrows(IllegalStateException.class, () -> sut.setRepository(repo));
        } finally {
            repo.shutDown();
        }
    }
}
