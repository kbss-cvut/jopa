package cz.cvut.kbss.jopa.query.soql;

import java.util.List;
import java.util.Objects;

sealed interface SoqlCondition {

    record Predicate(SoqlAttribute attribute) implements SoqlCondition {
        public Predicate {
            Objects.requireNonNull(attribute);
        }
    }

    record Conjunction(List<SoqlCondition> children) implements SoqlCondition {
        public Conjunction {
            children = List.copyOf(children);
        }
    }

    record Disjunction(List<SoqlCondition> children) implements SoqlCondition {
        public Disjunction {
            children = List.copyOf(children);
        }
    }

    record Negation(SoqlCondition child) implements SoqlCondition {
        public Negation {
            Objects.requireNonNull(child);
        }
    }
}
