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
package cz.cvut.kbss.jopa.loaders;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.function.Consumer;

import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class DefaultClasspathScannerTest {

    @Mock
    private Consumer<Class<?>> listener;

    @InjectMocks
    private DefaultClasspathScanner sut;

    @BeforeEach
    void setUp() {
        sut.addListener(listener);
    }

    @Test
    void processClassesFindsAllClassesOnClassPathWhenProvidedPackageIsEmpty() {
        sut.processClasses("");
        PersistenceUnitClassFinderTest.ENTITY_CLASSES.forEach(cls -> verify(listener).accept(cls));
    }
}
