package cz.cvut.kbss.jopa.owl2java.config;

import cz.cvut.kbss.jopa.owl2java.cli.CliParams;
import cz.cvut.kbss.jopa.owl2java.cli.CommandParserProvider;
import joptsimple.OptionSet;
import org.junit.Test;

import static org.junit.Assert.*;

public class TransformationConfigurationTest {

    @Test
    public void configIgnoresContextWhenWholeOntologyUsageIsConfigured() {
        final OptionSet optionSet = CommandParserProvider.getCommandTransform().parse("-i", "-c", "test");
        final TransformationConfiguration config = TransformationConfiguration.config(new CliParams(optionSet));
        assertNull(config.getContext());
    }
}