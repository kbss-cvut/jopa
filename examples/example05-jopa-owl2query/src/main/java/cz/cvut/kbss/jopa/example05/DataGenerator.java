package cz.cvut.kbss.jopa.example05;

import cz.cvut.kbss.jopa.example05.model.Superhero;

import java.util.HashMap;
import java.util.Map;

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
        kickAss.addAssociate(heroes.get("Red Mist"));
        heroes.put(kickAss.getNickname(), kickAss);
    }

    private void createBigDaddy() {
        final Superhero bigDaddy = new Superhero("Big Daddy");
        bigDaddy.setFirstName("Damon");
        bigDaddy.setLastName("Macready");
        bigDaddy.addAssociate(heroes.get("Hit-Girl"));
        heroes.put(bigDaddy.getNickname(), bigDaddy);
    }

    private void createHitGirl() {
        final Superhero hitGirl = new Superhero("Hit-Girl");
        hitGirl.setFirstName("Mindy");
        hitGirl.setLastName("Macready");
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
