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
