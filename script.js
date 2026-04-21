document.querySelectorAll('a[href^="#"]').forEach(anchor => {
    anchor.addEventListener('click', function(e) {
        e.preventDefault();
        document.querySelector(this.getAttribute('href')).scrollIntoView({
            behavior: 'smooth'
        });
    });
});

document.querySelectorAll('.abstract-toggle').forEach(btn => {
    const abstract = btn.closest('li').querySelector('.abstract-text');
    if (!abstract || !abstract.textContent.trim()) {
        btn.style.display = 'none';
        return;
    }
    btn.addEventListener('click', function() {
        abstract.classList.toggle('open');
        this.textContent = abstract.classList.contains('open') ? 'Abstract ▴' : 'Abstract ▾';
    });
});
