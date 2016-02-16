/**
 * Copyright (C) 2011 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.example05;

import cz.cvut.kbss.jopa.example05.model.Superhero;
import cz.cvut.kbss.jopa.example05.model.Vocabulary;

import java.util.HashMap;
import java.util.Map;

/**
 * Prepares some cool data.
 */
public class DataGenerator {

    private Map<String, Superhero> heroes = new HashMap<>();

    public Map<String, Superhero> generate() {
        createHeroes();
        return heroes;
    }

    private void createHeroes() {
        createRedMist();
        createKickAss();
        createHitGirl();
        createBigDaddy();
    }

    private void createKickAss() {
        final Superhero kickAss = new Superhero("Kick-Ass");
        kickAss.setFirstName("David");
        kickAss.setLastName("Lizewski");
        kickAss.addPropertyValue(Vocabulary.p_goodGuy, Boolean.TRUE.toString());
        kickAss.addAssociate(heroes.get("Red Mist"));
        heroes.put(kickAss.getNickname(), kickAss);
    }

    private void createBigDaddy() {
        final Superhero bigDaddy = new Superhero("Big Daddy");
        bigDaddy.setFirstName("Damon");
        bigDaddy.setLastName("Macready");
        bigDaddy.addPropertyValue(Vocabulary.p_goodGuy, Boolean.TRUE.toString());
        bigDaddy.addAssociate(heroes.get("Hit-Girl"));
        heroes.put(bigDaddy.getNickname(), bigDaddy);
    }

    private void createHitGirl() {
        final Superhero hitGirl = new Superhero("Hit-Girl");
        hitGirl.setFirstName("Mindy");
        hitGirl.setLastName("Macready");
        hitGirl.addPropertyValue(Vocabulary.p_goodGuy, Boolean.TRUE.toString());
        hitGirl.addAssociate(heroes.get("Kick-Ass"));
        heroes.put(hitGirl.getNickname(), hitGirl);
    }

    private void createRedMist() {
        final Superhero redMist = new Superhero("Red Mist");
        redMist.setFirstName("Chris");
        redMist.setLastName("D'Amico");
        heroes.put(redMist.getNickname(), redMist);
    }
}
