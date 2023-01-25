package cz.cvut.kbss.jopa.test.environment.util;

import cz.cvut.kbss.jopa.test.HasUri;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;

import java.util.Collection;
import java.util.Objects;

/**
 * Checks whether the provided collection contains the same entities as the expected one.
 * <p>
 * The membership check is done based on entity URIs.
 *
 * Item order is not significant in the comparison, but the total number of items is.
 */
public class ContainsSameEntities extends TypeSafeMatcher<Collection<? extends HasUri>> {

    private final Collection<? extends HasUri> expected;

    public ContainsSameEntities(Collection<? extends HasUri> expected) {
        this.expected = Objects.requireNonNull(expected);
    }

    @Override
    protected boolean matchesSafely(Collection<? extends HasUri> actual) {
        if (actual == null || actual.size() != expected.size()) {
            return false;
        }
        for (HasUri e : expected) {
            if (actual.stream().noneMatch(ee -> Objects.equals(e.getUri(), ee.getUri()))) {
                return false;
            }
        }
        return true;
    }

    @Override
    public void describeTo(Description description) {
        description.appendValueList("[", ", ", "]", expected);
    }

    public static ContainsSameEntities containsSameEntities(Collection<? extends HasUri> expected) {
        return new ContainsSameEntities(expected);
    }
}
