package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.Test;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Map;
import java.util.Set;

import static org.junit.Assert.*;

public class PendingAssertionsRegistryTest {

    private final NamedResource owner = NamedResource.create(Generators.createIndividualIdentifier());
    private final Assertion assertion =
            Assertion.createObjectPropertyAssertion(Generators.createPropertyIdentifier(), false);
    private final Object object = new Object();

    private PendingAssertionRegistry registry = new PendingAssertionRegistry();

    @Test
    public void addPendingAssertionAddsPendingAssertionToObjectsMap() throws Exception {
        registry.addPendingAssertion(owner, assertion, object, null);

        final Map<Object, Set<PendingAssertionRegistry.PendingAssertion>> assertions = getPendingAssertions();
        assertTrue(assertions.containsKey(object));
        final Set<PendingAssertionRegistry.PendingAssertion> pending = assertions.get(object);
        assertEquals(1, pending.size());
        final PendingAssertionRegistry.PendingAssertion pa = pending.iterator().next();
        assertEquals(owner, pa.getOwner());
        assertEquals(assertion, pa.getAssertion());
        assertNull(pa.getContext());
    }

    @Test
    public void addPendingAssertionAddsPendingAssertionToObjectsMapWithContext() throws Exception {
        final URI context = Generators.createIndividualIdentifier();
        registry.addPendingAssertion(owner, assertion, object, context);

        final Map<Object, Set<PendingAssertionRegistry.PendingAssertion>> assertions = getPendingAssertions();
        assertTrue(assertions.containsKey(object));
        final Set<PendingAssertionRegistry.PendingAssertion> pending = assertions.get(object);
        assertEquals(1, pending.size());
        final PendingAssertionRegistry.PendingAssertion pa = pending.iterator().next();
        assertEquals(owner, pa.getOwner());
        assertEquals(assertion, pa.getAssertion());
        assertEquals(context, pa.getContext());
    }

    private Map<Object, Set<PendingAssertionRegistry.PendingAssertion>> getPendingAssertions() throws Exception {
        final Field paField = PendingAssertionRegistry.class.getDeclaredField("pendingAssertions");
        paField.setAccessible(true);
        return (Map<Object, Set<PendingAssertionRegistry.PendingAssertion>>) paField.get(registry);
    }

    @Test
    public void removeAndGetRemovesPendingAssertionsForObjectAndReturnsThem() throws Exception {
        registry.addPendingAssertion(owner, assertion, object, null);
        final Map<Object, Set<PendingAssertionRegistry.PendingAssertion>> assertions = getPendingAssertions();
        final Set<PendingAssertionRegistry.PendingAssertion> pending = assertions.get(object);
        assertFalse(pending.isEmpty());

        final Set<PendingAssertionRegistry.PendingAssertion> result =
                registry.removeAndGetPendingAssertionsWith(object);
        assertEquals(pending, result);
        assertFalse(getPendingAssertions().containsKey(object));
    }
}