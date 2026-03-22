// Smooth scrolling for navigation links
document.querySelectorAll('a[href^="#"]').forEach(anchor => {
    anchor.addEventListener("click", function (e) {
        e.preventDefault();
        document.querySelector(this.getAttribute("href")).scrollIntoView({
            behavior: "smooth"
        });
    });
});

// Add hover effect to cards
document.querySelectorAll(".card").forEach(card => {
    card.addEventListener("mouseover", () => {
        card.style.transform = "scale(1.05)";
    });
    card.addEventListener("mouseout", () => {
        card.style.transform = "scale(1)";
    });
});

// Dynamic hero section background
const heroSection = document.querySelector(".hero");
const heroImages = [
    "https://source.unsplash.com/1600x900/?finance",
    "https://source.unsplash.com/1600x900/?business",
    "https://source.unsplash.com/1600x900/?investment"
];
let currentImageIndex = 0;

setInterval(() => {
    currentImageIndex = (currentImageIndex + 1) % heroImages.length;
    heroSection.style.backgroundImage = `url('${heroImages[currentImageIndex]}')`;
}, 5000);