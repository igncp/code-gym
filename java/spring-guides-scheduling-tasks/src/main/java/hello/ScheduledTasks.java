package hello;

import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.Random;
import java.util.ArrayList;

@Component
public class ScheduledTasks {

	private final Random rand = new Random();
	private final ArrayList<Integer> randomNums = new ArrayList<Integer>();

	@Scheduled(fixedRate = 1000)
	public void reportCurrentTime() {
		int newRandomNum = randInt(0, 100);
		System.out.println("The new random int 0-100 is: " + newRandomNum);
		randomNums.add(newRandomNum);
		System.out.println("All the random int are: " + randomNums);
	}

	private int randInt(int min, int max) {
		int randomNum = rand.nextInt((max - min) + 1) + min + 1;

		return randomNum;
	}
}
